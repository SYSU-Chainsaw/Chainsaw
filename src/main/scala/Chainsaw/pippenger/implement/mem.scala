package Chainsaw.pippenger.implement

import spinal.core._
import spinal.lib._

case class memPort[T <: Data](dataType: HardType[T], addressWidth: Int) extends Bundle with IMasterSlave {
  val address = UInt(addressWidth bits)
  val writeData = dataType()
  val readData = dataType()
  val we = Bool()
  val ce = Bool()

  override def asMaster(): Unit = {
    out(address, writeData, we, ce)
    in(readData)
  }
}

class mem[T <: Data](dataType: HardType[T], size: Int) extends Component {


  val io = new Bundle {
    val port = Vec(slave(memPort(dataType, log2Up(size))), 2)
  }

  val mem = Mem(dataType(), size) addAttribute("ram_style", "ultra")

  for (i <- 0 until 2) {
    io.port(i).readData := RegNext(mem.readWriteSync(io.port(i).address, io.port(i).writeData, io.port(i).ce, io.port(i).we, duringWrite = dontRead))
  }

  def latency: Int = 2
}