package Chainsaw.crypto

import Chainsaw._
import Chainsaw.arithmetic._
import Chainsaw.dag._

case class ModularAdd(widthIn: Int, constantModulus: Option[BigInt], sub: Boolean)
  extends Dag {
  val k = widthIn

  require(constantModulus.forall(_.bitLength == k))

  override def name = getAutoName(this)

  override def impl(dataIn: Seq[Any]) = {
    val data = dataIn.asInstanceOf[Seq[BigInt]]
    val ret = if (sub) (data(0) - data(1)).mod(constantModulus.get) else data.sum.mod(constantModulus.get)
    Seq(ret)
  }

  constantModulus match {
    case Some(value) =>
      val adder = Cpa(if (sub) BinarySubtractor else BinaryAdder, Seq(k), S2S, withCarry = true).asVertex
      val moder = (if (sub) FineReduction(value, 1, -1) else FineReduction(value, 2, 0)).asVertex

      val a, b = InputVertex(UIntInfo(k))
      val s = OutputVertex(UIntInfo(k))

      adder := (a, b)
      moder := adder.out(0)
      s := moder.out(0)
  }

  graphDone()
}
