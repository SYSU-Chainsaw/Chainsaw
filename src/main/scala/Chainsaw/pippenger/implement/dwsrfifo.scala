package Chainsaw.pippenger.implement

import spinal.core._
import spinal.lib._

class dwsrfifo[T <: Data](dataType: HardType[T], depth: Int) extends Component {
  //double write port single read port
  require(isPow2(depth))

  val io = new Bundle {
    val fromInput = slave Flow dataType()
    val fromAdder = slave Flow dataType()
    val toAdder = master Flow dataType()

    val empty = out Bool()
  }

  val mem = Array.fill(2)(Mem(dataType(), depth)) //0 adder 1 input
  val lvt = Reg(Bits(depth bits)) //false 0 true 1
  val pushPtr = Reg(UInt(log2Up(depth) + 1 bits)) init (0)
  val popPtr = Reg(UInt(log2Up(depth) + 1 bits)) init (0)

  val empty = pushPtr === popPtr
  io.empty := RegNext(empty) init (True)

  val pushArea = new Area {
    val lvtWriteEn = Bits(2 bits)
    val lvtWriteData = Bits(2 bits)

    lvtWriteEn(0) := io.fromAdder.valid | io.fromInput.valid
    lvtWriteEn(1) := io.fromAdder.valid & io.fromInput.valid
    lvtWriteData(0) := Mux(io.fromAdder.valid, False, True)
    lvtWriteData(1) := True

    val pushPtrNext = Vec(UInt(log2Up(depth) + 1 bits), 2)
    pushPtrNext(0) := pushPtr + io.fromAdder.valid.asUInt
    pushPtrNext(1) := pushPtrNext(0) + io.fromInput.valid.asUInt

    when(io.fromAdder.valid) {
      mem(0)(pushPtr.resized) := io.fromAdder.payload
    }
    when(io.fromInput.valid) {
      mem(1)(pushPtrNext(0).resized) := io.fromInput.payload
    }

    pushPtr := pushPtrNext.last

    switch(pushPtr(log2Up(depth) - 1 downto 0)) {
      for (i <- 0 until depth) {
        is(i) {
          for (j <- 0 until 2) {
            when(lvtWriteEn(j)) {
              lvt((i + j) % depth) := lvtWriteData(j)
            }
          }
        }
      }
    }
  }

  val popArea = new Area {
    io.toAdder.valid := RegNext(!empty) init (False)
    io.toAdder.payload := Mux(RegNext(lvt(popPtr.resized)), mem(1).readSync(popPtr.resized), mem(0).readSync(popPtr.resized))
    popPtr := popPtr + (!empty).asUInt
  }
}

object dwsrfifo extends App {
  //VivadoImpl(new dwsrfifo(UInt(253 bits), 1024))

  import spinal.core.sim._

  import scala.collection.mutable._

  SimConfig.withWave.compile(new dwsrfifo(UInt(8 bits), 16)).doSimUntilVoid { dut =>
    dut.clockDomain.forkStimulus(10)

    val queue = new Queue[Int]

    fork {
      while (true) {
        dut.io.fromAdder.randomize()
        dut.io.fromInput.valid.randomize()
        dut.io.fromInput.payload.randomize()
        dut.clockDomain.waitSampling()
        if (dut.io.fromAdder.valid.toBoolean) {
          queue.enqueue(dut.io.fromAdder.payload.toInt)
        }
        if (dut.io.fromInput.valid.toBoolean) {
          queue.enqueue(dut.io.fromInput.payload.toInt)
        }
      }
    }

    fork {
      for (i <- 0 until 100) {
        do {
          dut.clockDomain.waitSampling()
        } while (!dut.io.toAdder.valid.toBoolean)

        val output = dut.io.toAdder.payload.toInt
        val golden = queue.dequeue()

        if (output != golden) {
          print(s"i = ${i}时出错，从fifo中取得的数字为${output}，但正确的数字应该是${golden}。\n")
          simFailure()
        }
      }
      simSuccess()
    }
  }
}