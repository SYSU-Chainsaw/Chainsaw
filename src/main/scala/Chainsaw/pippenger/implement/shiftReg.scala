package Chainsaw.pippenger.implement

import spinal.core._
import spinal.lib._

class shiftReg[T <: Data](dataType: HardType[T], latency: Int) extends Component {

  val io = new Bundle {
    val dataIn = slave Flow dataType()
    val dataOut = master Flow dataType()
  }

  var shiftReg = io.dataIn
  for (i <- 0 until latency) {
    val temp = RegNext(shiftReg)
    temp.valid init (False)
    shiftReg \= temp
  }
  io.dataOut := shiftReg
}