package Chainsaw.pippenger.implement

import Chainsaw._
import Chainsaw.crypto._
import Chainsaw.project.zprize.ZPrizeMSM.baseModulus
import cc.redberry.rings.scaladsl._
import spinal.core._
import spinal.lib._

class pippengerWithAdder(pWidth: Int, N: Int, W: Int, w: Int) extends Component {

  val io = new Bundle {
    val inputData = slave Stream new Bundle {
      val k = UInt(W bits)
      val p = pType(pWidth)
    }

    val sum = master Flow pType(pWidth)
  }

  val adderDag = Array.fill(1)(new EllipticCurveAdder(new ShortWeierstrassCurve(baseModulus, 0, 1), pWidth, Option(baseModulus)))
  val pippenger = new pippenger(pType(pWidth), pType(pWidth).init, N, W, w, adderDag(0).latency)
  val adder = adderDag.map(_.implH)

  for (i <- 0 until 1) {
    adder(i).dataIn := Vec(pippenger.io.adderPort(i).a.asBits.subdivideIn(4 slices) ++ pippenger.io.adderPort(i).b.asBits.subdivideIn(4 slices))
    pippenger.io.adderPort(i).s.assignFromBits(adder(i).dataOut.asBits)
  }
  pippenger.io.adderPort(1).s.assignDontCare()

  pippenger.io.inputData.valid := io.inputData.valid
  pippenger.io.inputData.k := io.inputData.k
  pippenger.io.inputData.p := io.inputData.p
  io.inputData.ready := pippenger.io.inputData.ready

  io.sum << pippenger.io.sum
}

//object pippengerWithAdder extends App {
//  //SpinalVerilog(new pippengerWithAdder(377 * 3, 1 << 26, 253, 12, 300))
//
//  import spinal.core.sim._
//
//  import scala.collection.mutable._
//
//  val pWidth = 377 * 3
//  val N = 1 << 10
//  val W = 253
//  val w = 12
//  val latency = 150
//
//  val K = Array.fill(N)(BigInt(W, scala.util.Random))
//  val P = Array.fill(N)(BigInt(pWidth, scala.util.Random))
//  val Q = K.zip(P).map { case (k, p) => (k * p) % (BigInt(1) << pWidth) }
//  val S = Q.reduce((a, b) => (a + b) % (BigInt(1) << pWidth))
//
//  SimConfig.allOptimisation.compile(new pippengerWithAdder(pWidth, N, W, w, latency)).doSimUntilVoid { dut =>
//    dut.clockDomain.forkStimulus(10)
//
//    fork {
//      dut.io.inputData.valid #= true
//      for ((k, p) <- K.zip(P)) {
//        dut.io.inputData.k #= k
//        dut.io.inputData.p #= p
//        do {
//          dut.clockDomain.waitSampling()
//        } while (!dut.io.inputData.ready.toBoolean)
//      }
//      dut.io.inputData.valid #= false
//      while (true) {
//        dut.clockDomain.waitSampling()
//      }
//    }
//
//    fork {
//      do {
//        dut.clockDomain.waitSampling()
//      } while (!dut.io.sum.valid.toBoolean)
//      if (dut.io.sum.payload.toBigInt != S) {
//        print(s"发生错误，输出结果为${dut.io.sum.payload.toBigInt}，但正确结果应该是${S}。\n")
//      }
//      simSuccess()
//    }
//  }
//}