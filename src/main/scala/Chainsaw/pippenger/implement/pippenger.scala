package Chainsaw.pippenger.implement

import spinal.core._
import spinal.lib._
import spinal.lib.fsm._

class pippenger[T <: Data](pType: HardType[T], pInit: => T, N: Int, W: Int, w: Int, latency: Int) extends Component {

  //fifoDepth > latency ?
  require(w > 1)

  val io = new Bundle {
    val inputData = slave Stream new Bundle {
      val k = UInt(W bits)
      val p = pType()
    }

    val adderPort = Vec(master(new Bundle with IMasterSlave {
      val a = pType()
      val b = pType()
      val s = pType()

      override def asMaster(): Unit = {
        out(a, b)
        in(s)
      }
    }), 2)

    val sum = master Flow pType()
  }

  val G = Math.ceil(W.toDouble / w).toInt
  val init = pInit

  case class fifoType() extends Bundle {
    val a = pType()
    val b = pType()
    val address = UInt(log2Up((G + 1) / 2) + w bits)
  }

  val mem = Array.fill(2)(new mem(pType, (G + 1) / 2 << w))
  val fifo = Array.fill(2)(new dwsrfifo(fifoType(), 1 << log2Up(latency)))
  val stateRegs = Array.fill(2)(new stateReg((G + 1) / 2, w))

  val maxLatency = Math.max(Math.max(mem(0).latency, stateRegs(0).latency), 1)
  require(latency >= maxLatency)

  val shiftRegs = Array.fill(2)(new shiftReg(UInt(log2Up((G + 1) / 2) + w bits), latency - stateRegs(0).latency))

  require(maxLatency + latency - 2 * G - 1 > 0)

  io.inputData.setBlocked()
  io.adderPort.foreach { p =>
    p.a.assignDontCare()
    p.b.assignDontCare()
  }
  mem.foreach(_.io.port.foreach { p =>
    p.address.assignDontCare()
    p.writeData.assignDontCare()
    p.we := False
    p.ce := False
  })
  fifo.foreach { f =>
    f.io.fromInput.setIdle()
    f.io.fromAdder.setIdle()
  }
  stateRegs.foreach { s =>
    s.io.readAddress.assignDontCare()
    s.io.writeAddress.assignDontCare()
    s.io.writeEn.foreach(_ := False)
    s.io.softReset := False
  }
  shiftRegs.foreach(_.io.dataIn.setIdle())

  val outputValid = Reg(Bool()) init (False)
  outputValid := False

  io.sum.valid := outputValid
  io.sum.payload := io.adderPort(0).s

  val fsm = new StateMachine {
    setEncoding(binaryOneHot)

    val stage1 = makeInstantEntry()
    val stage2First = new State
    val stage2Work = new State
    val stage2Last = new State
    val stage3 = new State

    val stage1NCnt = Counter(N)
    val stage1WCnt = Counter((G + 1) / 2)
    val stage1WaitValid = Reg(Bool()) init (False)

    val stage1K = RegNext(Vec(io.inputData.k.resize(Math.ceil(W.toDouble / (2 * w)).toInt * (2 * w)).subdivideIn(2 * w bits)(stage1WCnt).subdivideIn(2 slices).map(stage1WCnt @@ _)))
    val stage1P = RegNext(io.inputData.p)
    val stage1KValid = Reg(Bool()) init (False)
    val adderOutputK = Vec(shiftRegs.map(_.io.dataOut.payload))
    val adderOutputValid = Vec(shiftRegs.map(_.io.dataOut.valid))

    val stage1KNext = Delay(stage1K, stateRegs(0).latency)
    val stage1PNext = Delay(stage1P, stateRegs(0).latency)
    val stage1KValidNext = Delay(stage1KValid, stateRegs(0).latency, init = False)
    val adderOutputKNext = Delay(adderOutputK, stateRegs(0).latency)
    val adderOutputP = Vec(io.adderPort.map(_.s))
    val adderOutputValidNext = Delay(adderOutputValid, stateRegs(0).latency,init =  Vec(False, 2))

    val kSame = Delay(Vec(stage1KValidNext & adderOutputValidNext(0) & stage1KNext(0) === adderOutputKNext(0) & stage1KNext(0) =/= 0,
      stage1KValidNext & adderOutputValidNext(1) & stage1KNext(1) === adderOutputKNext(1) & stage1KNext(1) =/= 0), mem(0).latency)
    val stage1KNextNext = Delay(stage1KNext, mem(0).latency)
    val stage1PNextNext = Delay(stage1PNext, mem(0).latency)
    val stage1KValidNextNext = Delay(stage1KValidNext, mem(0).latency, init = False)
    val adderOutputKNextNext = Delay(adderOutputKNext, mem(0).latency)
    val adderOutputPNext = Delay(adderOutputP, mem(0).latency)
    val adderOutputValidNextNext = Delay(adderOutputValidNext, mem(0).latency, init = Vec(False, 2))
    val stateRegsOutputNext = Delay(Vec(stateRegs.map(_.io.readData)), mem(0).latency)

    val emptyPeriod = latency + mem(0).latency + 1
    val leftDone = Reg(Bool()) init (False)
    val stage1LeftWaitCnt = Counter(emptyPeriod + 1)
    val rightDone = Reg(Bool()) init (False)
    val stage2RightWaitCnt = Counter(emptyPeriod + 1)

    stage1.whenIsActive {
      for (i <- 0 until 2) {
        stateRegs(i).io.readAddress(0) := stage1K(i)
        stateRegs(i).io.readAddress(1) := adderOutputK(i)

        stateRegs(i).io.writeAddress(0) := stage1K(i)
        stateRegs(i).io.writeAddress(1) := adderOutputK(i)

        stateRegs(i).io.writeEn(0) := stage1KValid
        stateRegs(i).io.writeEn(1) := adderOutputValid(i)

        mem(i).io.port(0).address := stage1KNext(i)
        mem(i).io.port(0).writeData := stage1PNext
        mem(i).io.port(0).ce := stage1KValidNext
        mem(i).io.port(0).we := !stateRegs(i).io.readData(0)

        mem(i).io.port(1).address := adderOutputKNext(i)
        mem(i).io.port(1).writeData := adderOutputP(i)
        mem(i).io.port(1).ce := adderOutputValidNext(i)
        mem(i).io.port(1).we := !stateRegs(i).io.readData(1)

        fifo(i).io.fromInput.valid := stage1KValidNextNext & stateRegsOutputNext(i)(0) & !kSame(i)
        fifo(i).io.fromInput.address := stage1KNextNext(i)
        fifo(i).io.fromInput.a := stage1PNextNext
        fifo(i).io.fromInput.b := mem(i).io.port(0).readData

        fifo(i).io.fromAdder.valid := (adderOutputValidNextNext(i) & (stateRegsOutputNext(i)(1) | kSame(i)))
        fifo(i).io.fromAdder.address := adderOutputKNextNext(i)
        fifo(i).io.fromAdder.a := adderOutputPNext(i)
        fifo(i).io.fromAdder.b := Mux(kSame(i), stage1PNextNext, mem(i).io.port(1).readData)

        io.adderPort(i).a := fifo(i).io.toAdder.a
        io.adderPort(i).b := fifo(i).io.toAdder.b
        shiftRegs(i).io.dataIn.valid := fifo(i).io.toAdder.valid
        shiftRegs(i).io.dataIn.payload := fifo(i).io.toAdder.address
      }

      when(stage1WaitValid) {
        //等待状态
        stage1KValid := False
        when(!leftDone) {
          when(fifo(0).io.empty) {
            stage1LeftWaitCnt.increment()
            when(stage1LeftWaitCnt.willOverflowIfInc) {
              leftDone := True
            }
          }.otherwise {
            stage1LeftWaitCnt.clear()
          }
        }
        when(!rightDone) {
          when(fifo(1).io.empty) {
            stage2RightWaitCnt.increment()
            when(stage2RightWaitCnt.willOverflowIfInc) {
              rightDone := True
            }
          }.otherwise {
            stage2RightWaitCnt.clear()
          }
        }
        when(leftDone & rightDone) {
          stage1WaitValid := False
          leftDone := False
          rightDone := False
          goto(stage2First)
        }
      }.otherwise {
        when(io.inputData.valid) {
          stage1WCnt.increment()
          stage1KValid := True
          when(stage1WCnt.willOverflowIfInc) {
            io.inputData.ready := True
            stage1NCnt.increment()
            when(stage1NCnt.willOverflowIfInc) {
              stage1WaitValid := True
            }
          }
        }.otherwise {
          stage1KValid := False
        }
      }
    }

    val stage2FirstCnt = Counter(G * 2)
    val stage2FirstCntLsb = Delay(stage2FirstCnt.lsb, maxLatency)
    val stage2FirstWaitValid = Reg(Bool()) init (False)

    val stateRegsReadAddress = UInt(log2Up(G) + w bits).assignDontCare()
    val stateRegsReadData = Delay(Mux(Delay(stateRegsReadAddress(w), stateRegs(0).latency), stateRegs(1).io.readData(0), stateRegs(0).io.readData(0)), maxLatency - stateRegs(0).latency)
    val memReadAddress = UInt(log2Up(G) + w bits).assignDontCare()
    val memReadEn = False
    val memReadDataBeforeDelay = Mux(Delay(memReadAddress(w), mem(0).latency), mem(1).io.port(0).readData, mem(0).io.port(0).readData)
    val memReadData = Delay(memReadDataBeforeDelay, maxLatency - mem(0).latency)

    val stage2DataReg = Reg(pType())

    val stage2FirstWaitCnt = Counter(maxLatency + latency - 2 * G)

    stage2First.whenIsActive {
      stateRegs.foreach(_.io.readAddress(0) := stateRegsReadAddress(log2Up(G) + w - 1 downto w + 1) @@ stateRegsReadAddress(w - 1 downto 0))
      mem.foreach { m =>
        m.io.port(0).address := memReadAddress(log2Up(G) + w - 1 downto w + 1) @@ memReadAddress(w - 1 downto 0)
        m.io.port(0).ce := memReadEn
      }
      stateRegsReadAddress := stage2FirstCnt(log2Up(G) downto 1) @@ Mux(stage2FirstCnt.lsb, U((1 << w) - 2, w bits), U((1 << w) - 1, w bits))
      memReadAddress := stage2FirstCnt(log2Up(G) downto 1) @@ Mux(stage2FirstCnt.lsb, U((1 << w) - 2, w bits), U((1 << w) - 1, w bits))
      io.adderPort(0).a := Mux(stage2FirstCntLsb, stage2DataReg, init)
      io.adderPort(0).b := Mux(stateRegsReadData, memReadData, init)
      when(!stage2FirstCntLsb) {
        stage2DataReg := Mux(stateRegsReadData, memReadData, init)
      }
      when(stage2FirstWaitValid) {
        stage2FirstWaitCnt.increment()
        when(stage2FirstWaitCnt.willOverflowIfInc) {
          stage2FirstWaitValid := False
          goto(stage2Work)
        }
      }.otherwise {
        stage2FirstCnt.increment()
        memReadEn := True
        when(stage2FirstCnt.willOverflowIfInc) {
          stage2FirstWaitValid := True
        }
      }
    }

    val stage2WorkWCnt = decreaseCounter((1 << w) - 2)
    val stage2WorkGCnt = Counter(2 * G)
    val stage2WorkGCntLsb = Delay(stage2WorkGCnt.lsb, maxLatency)
    val stage2WorkWaitValid = Reg(Bool()) init (False)

    val sDataReg1 = Delay(io.adderPort(0).s, maxLatency - 1)
    val sDataReg2 = RegNext(sDataReg1)

    val stage2WorkWaitCnt = Counter(maxLatency + latency - 2 * G)

    stage2Work.whenIsActive {
      stateRegs.foreach(_.io.readAddress(0) := stateRegsReadAddress(log2Up(G) + w - 1 downto w + 1) @@ stateRegsReadAddress(w - 1 downto 0))
      mem.foreach { m =>
        m.io.port(0).address := memReadAddress(log2Up(G) + w - 1 downto w + 1) @@ memReadAddress(w - 1 downto 0)
        m.io.port(0).ce := memReadEn
      }
      stateRegsReadAddress := stage2WorkGCnt(log2Up(G) downto 1) @@ stage2WorkWCnt
      memReadAddress := stage2WorkGCnt(log2Up(G) downto 1) @@ stage2WorkWCnt
      io.adderPort(0).a := Mux(stage2WorkGCntLsb, Mux(stateRegsReadData, memReadData, init), sDataReg1)
      io.adderPort(0).b := sDataReg2
      when(stage2WorkWaitValid) {
        stage2WorkWaitCnt.increment()
        when(stage2WorkWaitCnt.willOverflowIfInc) {
          stage2WorkWaitValid := False
          stage2WorkWCnt.decrement()
          when(stage2WorkWCnt.willUnderflowIfDec) {
            stateRegs.foreach(_.io.softReset := True)
            goto(stage2Last)
          }
        }
      }.otherwise {
        stage2WorkGCnt.increment()
        memReadEn := stage2WorkGCnt.lsb
        when(stage2WorkGCnt.willOverflowIfInc) {
          stage2WorkWaitValid := True
        }
      }
    }

    val stage2LastCnt = Counter(2 * (G - 1))

    val memWriteAddress = UInt(log2Up(G) + w bits).assignDontCare()
    val memWriteEn = False
    val memWriteData = pType().assignDontCare()

    stage2Last.whenIsActive {
      mem.foreach { m =>
        m.io.port(1).address := memWriteAddress(log2Up(G) + w - 1 downto w + 1) @@ memWriteAddress(w - 1 downto 0)
        m.io.port(1).writeData := memWriteData
      }
      mem(0).io.port(1).ce := memWriteEn & !memWriteAddress(w)
      mem(0).io.port(1).we := memWriteEn & !memWriteAddress(w)
      mem(1).io.port(1).ce := memWriteEn & memWriteAddress(w)
      mem(1).io.port(1).we := memWriteEn & memWriteAddress(w)
      memWriteAddress := stage2LastCnt(log2Up(G - 1) downto 1).resize(log2Up(G)) @@ U(0, w bits)
      memWriteEn := !stage2LastCnt.lsb
      memWriteData := io.adderPort(0).s

      stage2LastCnt.increment()
      when(stage2LastCnt.willOverflowIfInc) {
        goto(stage3)
      }
    }

    val stage3GCnt = decreaseCounter(G - 1)
    val stage3AddValid = Reg(Bool()) init (False)
    val stage3AddValidDelay = Delay(stage3AddValid, mem(0).latency)
    val stage3DoubleCnt = Counter(w)
    val stage3DoubleWaitValid = Reg(Bool()) init (False)
    val stage3AddWaitValid = Reg(Bool()) init (False)

    val stage3DoubleWaitCnt = Counter(mem(0).latency + latency - 1)
    val stage3AddWaitCnt = Counter(mem(0).latency + latency - 1)

    val sDataReg3 = Delay(io.adderPort(0).s, mem(0).latency)

    stage3.whenIsActive {
      mem.foreach { m =>
        m.io.port(0).address := memReadAddress(log2Up(G) + w - 1 downto w + 1) @@ memReadAddress(w - 1 downto 0)
        m.io.port(0).ce := memReadEn
      }
      memReadAddress := stage3GCnt.resize(log2Up(G)) @@ U(0, w bits)
      io.adderPort(0).a := Mux(stage3AddValidDelay, memReadDataBeforeDelay, sDataReg3)
      io.adderPort(0).b := sDataReg3
      when(stage3AddValid) {
        when(stage3AddWaitValid) {
          stage3AddWaitCnt.increment()
          when(stage3AddWaitCnt.willOverflowIfInc) {
            stage3AddWaitValid := False
            stage3AddValid := False
            stage3GCnt.decrement()
            when(stage3GCnt.willUnderflowIfDec) {
              outputValid := True
              goto(stage1)
            }
          }
        }.otherwise {
          memReadEn := True
          stage3AddWaitValid := True
        }
      }.otherwise {
        when(stage3DoubleWaitValid) {
          stage3DoubleWaitCnt.increment()
          when(stage3DoubleWaitCnt.willOverflowIfInc) {
            stage3DoubleWaitValid := False
            stage3DoubleCnt.increment()
            when(stage3DoubleCnt.willOverflowIfInc) {
              stage3AddValid := True
            }
          }
        }.otherwise {
          stage3DoubleWaitValid := True
        }
      }
    }
  }
}

object pippenger extends App {
  //SpinalVerilog(new pippenger(UInt(377 bits), U(0, 377 bits), 1 << 26, 253, 10, 300)).printPruned()

  import spinal.core.sim._

  import scala.collection.mutable._

  val pWidth = 377
  val N = 1 << 10
  val W = 253
  val w = 10
  val latency = 300

  val K = Array.fill(N)(BigInt(W, scala.util.Random))
  val P = Array.fill(N)(BigInt(pWidth, scala.util.Random))
  val Q = K.zip(P).map { case (k, p) => (k * p) % (BigInt(1) << pWidth) }
  val S = Q.reduce((a, b) => (a + b) % (BigInt(1) << pWidth))

  SimConfig.allOptimisation.compile(new pippenger(UInt(pWidth bits), U(0, pWidth bits), N, W, w, latency)).doSimUntilVoid { dut =>
    dut.clockDomain.forkStimulus(10)

    fork {
      dut.io.inputData.valid #= true
      for ((k, p) <- K.zip(P)) {
        dut.io.inputData.k #= k
        dut.io.inputData.p #= p
        do {
          dut.clockDomain.waitSampling()
        } while (!dut.io.inputData.ready.toBoolean)
      }
      dut.io.inputData.valid #= false
      while (true) {
        dut.clockDomain.waitSampling()
      }
    }

    fork {
      val queue = Array.fill(2)(Queue.fill(latency - 1)(BigInt(0)))

      while (true) {
        for (i <- 0 until 2) {
          queue(i).enqueue((dut.io.adderPort(i).a.toBigInt + dut.io.adderPort(i).b.toBigInt) % (BigInt(1) << pWidth))
          dut.io.adderPort(i).s #= queue(i).dequeue()
        }
        dut.clockDomain.waitSampling()
      }
    }

    fork {
      do {
        dut.clockDomain.waitSampling()
      } while (!dut.io.sum.valid.toBoolean)
      if (dut.io.sum.payload.toBigInt != S) {
        print(s"发生错误，输出结果为${dut.io.sum.payload.toBigInt}，但正确结果应该是${S}。\n")
      }
      simSuccess()
    }
  }
}