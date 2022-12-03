package Chainsaw.crypto

import Chainsaw._
import Chainsaw.xilinx._

import scala.util.Random
import org.scalatest.flatspec.AnyFlatSpec

class ModularAddTest extends ChainsawFlatSpec {

  val Ms = Seq.fill(3)(Some(randBigInt(376) + Pow2(376))) // :+ None TODO: variable modulus
  val adderTypes = Seq(BinaryAdder) // BinarySubtractor,TernaryAdder, TernarySubtractor1, TernarySubtractor2 TODO: more adder modes

  Ms.foreach(M =>
    adderTypes.foreach(adderType =>
      testGenerator(ModularAdd(377, M, false))
    ))


}
