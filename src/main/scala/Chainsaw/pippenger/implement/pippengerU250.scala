package Chainsaw.pippenger.implement

import spinal.core._
import spinal.lib._
import spinal.lib.bus.amba4.axi._
import spinal.lib.bus.amba4.axilite._
import spinal.lib.fsm._

class pippengerU250 extends Component {

  val io = new Bundle {

    val dataBus = master(Axi4(Axi4Config(64, 512, useId = false, useRegion = false,
      useBurst = false, useLock = false, useCache = false, useSize = false, useQos = false,
      useLen = true, useLast = true, useResp = false, useProt = false, useStrb = false)))

    val s_axi_control = slave(AxiLite4(AxiLite4Config(12, 32)))

    Axi4SpecRenamer(dataBus)
    AxiLite4SpecRenamer(s_axi_control)
  }

  val pWidth = 377
  val N = 1 << 26
  val W = 253
  val w = 8

  val pippenger = new pippengerWithAdder(pWidth, N, W, w)

  val start = Reg(Bool()) init (False) //0x10, 0
  val groupReady = Reg(Bits(2 bits)) init (0) //0x10, 1 and 2
  val periodCnt = Counter(32 bits) // 0x14
  val startReadAddress = Vec(Reg(UInt(64 bits)), 2) //0x20
  val startWriteAddress = Reg(UInt(64 bits)) //0x30

  when(start.rise()) {
    periodCnt.clear()
  }.elsewhen(start) {
    periodCnt.increment()
  }

  val startRise = False
  val startFall = False
  when(startRise) {
    start := True
  }.elsewhen(startFall) {
    start := False
  }

  val groupReadyRise = Vec(False, 2)
  val groupReadyFall = Vec(False, 2)
  for (i <- 0 until 2) {
    when(groupReadyRise(i)) {
      groupReady(i) := True
    }.elsewhen(groupReadyFall(i)) {
      groupReady(i) := False
    }
  }

  //数据格式：每2048个bit存储一个数据
  //共有1 << 26个数据，共16GB
  //将其分割为64个大小为256M的数据块

  io.dataBus.ar.setIdle()
  io.dataBus.r.setBlocked()
  io.dataBus.aw.setIdle()
  io.dataBus.w.setIdle()
  io.dataBus.b.ready := True

  val dataBusReadArea = new Area {
    val inputCnt = Counter(4)
    val inputValid = Reg(Bool()) init (False)
    val inputData = Vec(Reg(UInt(512 bits)), 4)

    case class inputDataType() extends Bundle {
      val k = UInt(W bits)
      val p = pType(pWidth)
    }

    val inputFifo = StreamFifo(inputDataType(), 8)
    pippenger.io.inputData.valid := inputFifo.io.pop.valid
    pippenger.io.inputData.k := inputFifo.io.pop.k
    pippenger.io.inputData.p := inputFifo.io.pop.p
    inputFifo.io.pop.ready := pippenger.io.inputData.ready

    inputFifo.io.push.valid := inputValid
    inputFifo.io.push.payload.assignFromBits(inputData.asBits.resize(inputFifo.io.push.payload.getBitsWidth))

    val dataFifo = new StreamFifo(UInt(512 bits), 128)
    dataFifo.io.push.setIdle()
    dataFifo.io.pop.ready := !inputValid | inputFifo.io.push.ready
    val dataFifoReady = RegNext(!dataFifo.io.occupancy(6))

    when(inputCnt.willOverflow) {
      inputValid := True
    }.elsewhen(inputFifo.io.push.ready) {
      inputValid := False
    }

    when(dataFifo.io.pop.fire) {
      inputCnt.increment()
      for (i <- 0 until 3) {
        inputData(i) := inputData(i + 1)
      }
      inputData.last := dataFifo.io.pop.payload
    }
  }

  val dataBusReadFsm = new StateMachine {
    val waitStart = makeInstantEntry()
    val waitGroup = new State
    val waitReady = new State
    val writeAddress = new State
    val readData = new State
    val done = new State

    val readAddressNow = Reg(UInt(64 bits))
    val groupCnt = Counter(64) //组数
    val pageCnt = Counter(65536) //每组的页数

    waitStart.whenIsActive {
      when(start) {
        goto(waitGroup)
      }
    }

    waitGroup.whenIsActive {
      when(groupReady(groupCnt(0 downto 0))) {
        readAddressNow := startReadAddress(groupCnt(0 downto 0))
        goto(waitReady)
      }
    }

    waitReady.whenIsActive {
      when(dataBusReadArea.dataFifoReady) {
        goto(writeAddress)
      }
    }

    writeAddress.whenIsActive {
      io.dataBus.ar.valid := True
      io.dataBus.ar.addr := readAddressNow
      io.dataBus.ar.len := 64 - 1
      when(io.dataBus.ar.ready) {
        goto(readData)
      }
    }

    readData.whenIsActive {
      io.dataBus.r.ready := True
      dataBusReadArea.dataFifo.io.push.valid := io.dataBus.r.valid
      dataBusReadArea.dataFifo.io.push.payload := io.dataBus.r.data.asUInt
      when(io.dataBus.r.valid & io.dataBus.r.last) {
        readAddressNow := readAddressNow + (1 << 12)
        pageCnt.increment()
        when(RegNext(pageCnt.valueNext === pageCnt.maxValue)) {
          groupCnt.increment()
          groupReadyFall(groupCnt(0 downto 0)) := True
          when(RegNext(groupCnt.valueNext === groupCnt.maxValue)) {
            goto(done)
          }.otherwise {
            goto(waitGroup)
          }
        }.otherwise {
          goto(waitReady)
        }
      }
    }

    done.whenIsActive {
      when(!start) {
        goto(waitStart)
      }
    }
  }

  val dataBusWriteFsm = new StateMachine {
    val waitWrite = makeInstantEntry()
    val writeAddress = new State
    val writeData = new State
    val done = new State

    val data = Reg(Bits(pippenger.io.sum.payload.getBitsWidth bits))
    val writeCnt = Counter(4)

    waitWrite.whenIsActive {
      when(pippenger.io.sum.valid) {
        data := pippenger.io.sum.payload.asBits
        goto(writeAddress)
      }
    }

    writeAddress.whenIsActive {
      io.dataBus.aw.valid := True
      io.dataBus.aw.addr := startWriteAddress
      io.dataBus.aw.len := 4 - 1
      when(io.dataBus.aw.ready) {
        goto(writeData)
      }
    }

    writeData.whenIsActive {
      io.dataBus.w.valid := True
      io.dataBus.w.data := data.asBits.resized
      io.dataBus.w.last := RegNext(writeCnt.valueNext === 3)
      when(io.dataBus.w.ready) {
        writeCnt.increment()
        data := data |>> 512
        when(io.dataBus.w.last) {
          goto(done)
        }
      }
    }

    done.whenIsActive {
      startFall := True
      goto(waitWrite)
    }
  }

  io.s_axi_control.aw.setBlocked()
  io.s_axi_control.w.setBlocked()
  io.s_axi_control.b.setIdle()
  io.s_axi_control.ar.setBlocked()
  io.s_axi_control.r.setIdle()

  val ctrlBusReadFsm = new StateMachine {
    val waitAddress = makeInstantEntry()
    val getData = new State
    val readData = new State

    val address = Reg(UInt(12 bits))

    waitAddress.whenIsActive {
      io.s_axi_control.ar.ready := True
      when(io.s_axi_control.ar.valid) {
        address := io.s_axi_control.ar.addr
        goto(getData)
      }
    }

    val data = Reg(UInt(32 bits))

    getData.whenIsActive {
      switch(address) {
        is(0x10, 0x11, 0x12, 0x13) {
          data := (groupReady ## start).asUInt.resized
        }
        is(0x14, 0x15, 0x16, 0x17) {
          data := periodCnt
        }
        is(0x20, 0x21, 0x22, 0x23) {
          data := startReadAddress(0)(31 downto 0)
        }
        is(0x24, 0x25, 0x26, 0x27) {
          data := startReadAddress(0)(63 downto 32)
        }
        is(0x28, 0x29, 0x2a, 0x2b) {
          data := startReadAddress(1)(31 downto 0)
        }
        is(0x2c, 0x2d, 0x2e, 0x2f) {
          data := startReadAddress(1)(63 downto 32)
        }
        is(0x30, 0x31, 0x32, 0x33) {
          data := startWriteAddress(31 downto 0)
        }
        is(0x34, 0x35, 0x36, 0x37) {
          data := startWriteAddress(63 downto 32)
        }
      }
      goto(readData)
    }

    readData.whenIsActive {
      io.s_axi_control.r.valid := True
      io.s_axi_control.r.setOKAY()
      io.s_axi_control.r.data := data.asBits
      when(io.s_axi_control.r.ready) {
        goto(waitAddress)
      }
    }
  }

  val ctrlBusWriteFsm = new StateMachine {
    val waitAddress = makeInstantEntry()
    val getData = new State
    val writeData = new State
    val resp = new State

    val address = Reg(UInt(12 bits))

    waitAddress.whenIsActive {
      io.s_axi_control.aw.ready := True
      when(io.s_axi_control.aw.valid) {
        address := io.s_axi_control.aw.addr
        goto(getData)
      }
    }

    val data = Reg(Bits(32 bits))

    getData.whenIsActive {
      io.s_axi_control.w.ready := True
      when(io.s_axi_control.w.valid) {
        data := io.s_axi_control.w.data
        goto(writeData)
      }
    }

    writeData.whenIsActive {
      switch(address) {
        is(0x10, 0x11, 0x12, 0x13) {
          startRise := data(0)
          groupReadyRise := data(2 downto 1).asBools
        }
        is(0x20, 0x21, 0x22, 0x23) {
          startReadAddress(0)(31 downto 0) := data.asUInt
        }
        is(0x24, 0x25, 0x26, 0x27) {
          startReadAddress(0)(63 downto 32) := data.asUInt
        }
        is(0x28, 0x29, 0x2a, 0x2b) {
          startReadAddress(1)(31 downto 0) := data.asUInt
        }
        is(0x2c, 0x2d, 0x2e, 0x2f) {
          startReadAddress(1)(63 downto 32) := data.asUInt
        }
        is(0x30, 0x31, 0x32, 0x33) {
          startWriteAddress(31 downto 0) := data.asUInt
        }
        is(0x34, 0x35, 0x36, 0x37) {
          startWriteAddress(63 downto 32) := data.asUInt
        }
      }
      goto(resp)
    }

    resp.whenIsActive {
      io.s_axi_control.b.valid := True
      io.s_axi_control.b.setOKAY()
      when(io.s_axi_control.b.ready) {
        goto(waitAddress)
      }
    }
  }
}

object pippengerU250 extends App {
  SpinalVerilog(SpinalConfig(mode = Verilog, defaultConfigForClockDomains = ClockDomainConfig(resetActiveLevel = LOW)))(new pippengerU250).printPruned()
}