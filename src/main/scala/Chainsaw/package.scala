
import org.slf4j.LoggerFactory
import spinal.core._
import spinal.lib._

import java.io.File
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.reflect.ClassTag

package object Chainsaw {

  /** --------
   * global run-time environment
   * -------- */
  val logger = LoggerFactory.getLogger("Chainsaw logger") // global logger
  var verbose = 0

  val naiveSet = mutable.Set[String]()
  var atSimTime = true

  val dot = "■"

  /** --------
   * type def
   * -------- */
  type Metric = (Any, Any) => Boolean
  type FrameMetric = (Seq[Any], Seq[Any]) => Boolean

  /** --------
   * paths
   * -------- */
  val vivadoPath = new File("/tools/Xilinx/Vivado/2021.1/bin/vivado") // vivado executable path TODO: should be read from environment variables
  val quartusDir = new File("/tools/quartus/bin")
  val unisimDir = new File("src/main/resources/unisims")
  val simWorkspace = new File("simWorkspace")
  val synthWorkspace = new File("synthWorkspace")

  /** --------
   * scala type utils
   * -------- */
  implicit class IntUtil(int: Int) {
    def divideAndCeil(base: Int) = (int + base - 1) / base

    def nextMultiple(base: Int) = divideAndCeil(base) * base
  }

  implicit class StringUtil(s: String) {

    // complement version of method padTo(padToRight)
    def padToLeft(len: Int, elem: Char) = s.reverse.padTo(len, elem).reverse

    def repeat(times: Int) = Seq.fill(times)(s).reduce(_ + _)
  }

  implicit class seqUtil[T: ClassTag](seq: Seq[T]) {
    def prevAndNext[TOut](f: ((T, T)) => TOut) = seq.init.zip(seq.tail).map(f)

    def padToLeft(len: Int, elem: T) = seq.reverse.padTo(len, elem).reverse
  }

  case class BitValue(value: BigInt, width: Int) {

    /** works the same as SpinalHDL splitAt
     *
     * @example 10100.split(3) = (10,100)
     */
    def splitAt(lowWidth: Int): (BigInt, BigInt) = {
      require(value > 0)
      val base = BigInt(1) << lowWidth
      (value >> lowWidth, value % base)
    }

    def takeLow(n: Int) = {
      require(value >= BigInt(0))
      splitAt(n)._2
    }

    def takeHigh(n: Int) = {
      require(value >= BigInt(0))
      splitAt(value.bitLength - n)._1
    }

    def apply(range: Range) = {
      (value / Pow2(range.low)) % Pow2(range.length)
    }
  }

  // TODO: make BigInt behaves just like Bits/UInt
  implicit class BigIntUtil(bi: BigInt) {
    def toBitValue(width: Int = -1) = {
      if (width == -1) BitValue(bi, bi.bitLength)
      else BitValue(bi, width)
    }
  }

  /** --------
   * spinal type utils
   * -------- */
  // extension of Data
  implicit class DataUtil[T <: Data](data: T) {
    def d(cycle: Int = 1): T = Delay(data, cycle)
  }

  // extension of Bool
  implicit class BoolUtil(data: Bool) {
    // drive a flag which is initially unset
    def validAfter(cycle: Int): Bool = Delay(data, cycle, init = False)
  }

  implicit class VecUtil[T <: Data](vec: Vec[T]) {
    def :=(that: Seq[T]): Unit = {
      require(vec.length == that.length)
      vec.zip(that).foreach { case (port, data) => port := data }
    }

    def vecShiftWrapper(bitsShift: UInt => Bits, that: UInt): Vec[T] = {
      val ret = cloneOf(vec)
      val shiftedBits: Bits = bitsShift((that * widthOf(vec.dataType)).resize(log2Up(widthOf(vec.asBits))))
      ret.assignFromBits(shiftedBits)
      ret
    }

    val bits = vec.asBits

    def rotateLeft(that: Int): Vec[T] = vecShiftWrapper(bits.rotateRight, that)

    def rotateLeft(that: UInt): Vec[T] = vecShiftWrapper(bits.rotateRight, that)

    def rotateRight(that: Int): Vec[T] = vecShiftWrapper(bits.rotateLeft, that)

    def rotateRight(that: UInt): Vec[T] = vecShiftWrapper(bits.rotateLeft, that)
  }


  object Pow2 {
    def apply(exp: Int) = BigInt(1) << exp
  }

  import xilinx._

  def ChainsawSynth(gen: ChainsawGenerator, name: String, withRequirement: Boolean = false) = {
    val report = VivadoSynth(gen.implH, name)
    if (withRequirement) report.require(gen.utilEstimation, gen.fmaxEstimation)
    report
  }

}
