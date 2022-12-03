package Chainsaw.pippenger.implement

import spinal.core._

case class pType(pWidth: Int) extends Bundle {
  val X = UInt(pWidth bits)
  val Y = UInt(pWidth bits)
  val Z = UInt(pWidth bits)
  val T = UInt(pWidth bits)

  def init: this.type = {
    X := 0
    Y := 1
    Z := 1
    T := 0
    this
  }
}