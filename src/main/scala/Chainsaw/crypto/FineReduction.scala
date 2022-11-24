package Chainsaw.crypto

import Chainsaw._
import Chainsaw.arithmetic._
import cc.redberry.rings.scaladsl._
import spinal.core._

import scala.collection.mutable.ArrayBuffer
import scala.language.postfixOps

/**
 * @param M
 * @param upperBound
 * @param lowerBound
 * @see ''Langhammer, Martin and Bogdan Mihai Pasca. “Efficient FPGA Modular Multiplication Implementation.” The 2021 ACM/SIGDA International Symposium on Field-Programmable Gate Arrays (2021): n. pag.''
 */
case class FineReduction(M: BigInt, upperBound: Int, lowerBound: Int = 0) extends ChainsawGenerator {
  require(upperBound > lowerBound)

  override def name = s"FineReduction_upper_${upperBound.toString.replace("-", "Neg")}_lower_${lowerBound.toString.replace("-", "Neg")}"


  override def impl(dataIn: Seq[Any]): Seq[BigInt] = {
    val ret = Zp(M)(dataIn.head.asInstanceOf[BigInt]).toBigInt
    Seq(ret)
  }

  val hasPos = upperBound > 0
  val hasNeg = lowerBound < 0

  val bypassCond = upperBound == 1 && lowerBound == 0
  val simpleAddCond = upperBound == 1 && lowerBound == -1
  val simpleSubCond = upperBound == 2 && lowerBound == 0
  val simpleAddSubCond = upperBound == 2 && lowerBound == -1
  val otherSimpleCond = upperBound - lowerBound == 1 && !bypassCond
  val simpleCond = simpleAddCond || simpleSubCond || simpleAddSubCond || otherSimpleCond

  val min = lowerBound * M
  val max = upperBound * M - 1

  val k = M.bitLength
  val widthIn = log2Up(Array((min - 1).abs, max.abs).max)
  val widthAdder = if (simpleAddCond || otherSimpleCond) k else k + 1
  val widthOut = k
  val highWidth = widthIn + 2 - widthOut

  val add0Gen = if (bypassCond) null else Cpa(BinaryAdder, Seq(widthAdder), S2S, withCarry = false)
  val add1Gen = if (bypassCond || simpleCond) null else Cpa(BinaryAdder, Seq(widthAdder), S2S, withCarry = false)

  val map = new ArrayBuffer[(BigInt, BigInt, BigInt)]

  if (lowerBound < 0) {
    for (i <- (min >> (k - 1)) to Array(BigInt(-1), max >> (k - 1)).min) {
      val c = (i << (k - 1)) / M
      map += Tuple3(i & ((1 << highWidth) - 1), -c, -c + 1)
    }
    for (i <- BigInt(0) to (max >> (k - 1))) {
      val c = (i << (k - 1)) / M
      map += Tuple3(i, -c - 1, -c)
    }
  } else {
    for (i <- Array(BigInt(0), min >> (k - 1)).min to (max >> (k - 1))) {
      val c = (i << (k - 1)) / M
      map += Tuple3(i, -c - 1, -c)
    }
  }

  override var inputTypes = Seq(SIntInfo(widthIn))
  override var outputTypes = Seq(UIntInfo(widthOut))

  override var inputFormat = inputNoControl
  override var outputFormat = outputNoControl
  override var latency = if (bypassCond) 0
  else if (simpleAddCond || simpleSubCond || simpleAddSubCond) add0Gen.latency + 1
  else if (otherSimpleCond) add0Gen.latency
  else add0Gen.latency + 2

  override def implH: ChainsawModule = new ChainsawModule(this) {

    val bypass = bypassCond generate new Area {
      //upperBound == 1, lowerBound == 0
      dataOut.head := dataIn.head.resize(widthOut)
    }

    val simpleAdd = simpleAddCond generate new Area {
      //upperBound == 1, lowerBound == -1
      dataOut.head := add0Gen.asFunc(Seq(dataIn.head.resize(widthAdder).d(), Mux(dataIn.head.msb, U(M, widthAdder bits), U(0, widthAdder bits)).asBits.d())).head
    }

    val simpleSub = simpleSubCond generate new Area {
      //upperBound == 2, lowerBound == 0
      val ret0 = add0Gen.asFunc(Seq(dataIn.head.resize(widthAdder), S(-M).resize(widthAdder).asBits)).head
      dataOut.head := Mux(ret0.msb, dataIn.head.resize(widthOut).d(add0Gen.latency), ret0.resize(widthOut)).d()
    }

    val simpleAddSub = simpleAddSubCond generate new Area {
      //upperBound == 2, lowerBound == -1
      val ret0 = add0Gen.asFunc(Seq(dataIn.head.resize(widthAdder), Mux(dataIn.head.msb, S(M).resize(widthAdder), S(-M).resize(widthAdder)).asBits)).head
      dataOut.head := Mux(dataIn.head.msb.d(add0Gen.latency), ret0.resize(widthOut), Mux(ret0.msb, dataIn.head.resize(widthOut).d(add0Gen.latency), ret0.resize(widthOut))).d()
    }

    val otherSimple = otherSimpleCond generate new Area {
      //upperBound - lowerBound == 1
      dataOut.head := add0Gen.asFunc(Seq(dataIn.head.resize(widthAdder), S(-lowerBound * M).resize(widthAdder).asBits)).head
    }

    val fullCond = !(bypassCond || simpleCond) generate new Area {
      val add0In = Bits(widthAdder bits).assignDontCare().allowOverride
      val add1In = Bits(widthAdder bits).assignDontCare().allowOverride
      switch(dataIn.head.takeHigh(highWidth)) {
        map.foreach { m =>
          is(m._1) {
            add0In := S(m._2 * M).resize(widthAdder bits).asBits
            add1In := S(m._3 * M).resize(widthAdder bits).asBits
          }
        }
      }
      val ret0 = add0Gen.asFunc(Seq(dataIn.head.resize(widthAdder).d(), add0In.d())).head
      val ret1 = add1Gen.asFunc(Seq(dataIn.head.resize(widthAdder).d(), add1In.d())).head

      dataOut.head := Mux(ret0.msb, ret1.resize(widthOut), ret0.resize(widthOut)).d()
    }
  }
}

