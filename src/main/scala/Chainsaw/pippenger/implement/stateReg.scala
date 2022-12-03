package Chainsaw.pippenger.implement

import spinal.core._
import spinal.lib._

class stateReg(G: Int, w: Int) extends Component {

  val io = new Bundle {
    val readAddress = Vec(in UInt (log2Up(G) + w bits), 2)
    val readData = Vec(out Bool(), 2)

    val writeAddress = Vec(in UInt (log2Up(G) + w bits), 2)
    val writeEn = Vec(in Bool(), 2)

    val softReset = in Bool()
  }

  val regs = Vec(Reg(Bits((1 << w) - 1 bits)) init (0), G)

  val writeArea = new Area {
    val writeNetworks = Array.fill(2)(new writeNetwork(G, w))

    val flipAvailAble = Vec(Vec(Bits((1 << w) bits), G), 2)
    for (i <- 0 until 2) {
      writeNetworks(i).io.writeAddress := io.writeAddress(i)
      writeNetworks(i).io.writeEn := io.writeEn(i)
      flipAvailAble(i) := writeNetworks(i).io.writeData
    }

    val regsNext = Vec(regs.zip(flipAvailAble.head.zip(flipAvailAble.last)).map { case (r, (f1, f2)) => r ^ f1((1 << w) - 1 downto 1) ^ f2((1 << w) - 1 downto 1) })

    when(io.softReset) {
      regs.foreach(_.clearAll())
    }.otherwise {
      regs := regsNext
    }
  }

  val readArea = new Area {
    val readNetworks = Array.fill(2)(new readNetwork(G, w))
    for (i <- 0 until 2) {
      readNetworks(i).io.readAddress := Delay(io.readAddress(i), writeArea.writeNetworks.head.latency)
      readNetworks(i).io.readRegs := Vec(regs.map(_ ## False))
      io.readData(i) := readNetworks(i).io.readData
    }
  }

  def latency: Int = writeArea.writeNetworks.head.latency + readArea.readNetworks.head.latency
}

class writeNetwork(G: Int, w: Int) extends Component {
  val io = new Bundle {
    val writeAddress = in UInt (log2Up(G) + w bits)
    val writeEn = in Bool()

    val writeData = out Vec(Bits(1 << w bits), G)
  }

  val addressSlice = Vec(io.writeAddress.subdivideIn(8 bits, false).zipWithIndex.map(a => Delay(a._1, a._2)))
  val dataArray = new Array[Vec[Bits]](addressSlice.length + 1)
  dataArray(0) = Vec(io.writeEn.asBits)

  for (i <- addressSlice.indices) {
    dataArray(i + 1) = if (dataArray(i).getBitsWidth << addressSlice(i).getBitsWidth <= (1 << w))
      Vec(Reg(Bits(dataArray(i).getBitsWidth << addressSlice(i).getBitsWidth bits)) init (0)) else
      Vec(Reg(Bits(1 << w bits)) init (0), dataArray(i).getBitsWidth << addressSlice(i).getBitsWidth >> w)
    if (dataArray(i + 1).length < (1 << addressSlice(i).getBitsWidth)) {
      val length = (1 << addressSlice(i).getBitsWidth) / dataArray(i + 1).length
      for (j <- 0 until (1 << addressSlice(i).getBitsWidth)) {
        dataArray(i + 1)(j / length).subdivideIn(dataArray(i).head.getBitsWidth bits)(j % length) := Mux(addressSlice(i) === j, dataArray(i).head, B(0, dataArray(i).head.getBitsWidth bits))
      }
    } else {
      for (j <- 0 until (1 << addressSlice(i).getBitsWidth)) {
        Vec(dataArray(i + 1).slice(j * dataArray(i).length, (j + 1) * dataArray(i).length)) := Mux(addressSlice(i) === j, dataArray(i), Vec(B(0, (1 << w) bits), dataArray(i).length))
      }
    }
  }

  io.writeData := Vec(dataArray.last.slice(0, G))

  def latency: Int = addressSlice.length
}

class readNetwork(G: Int, w: Int) extends Component {

  val io = new Bundle {
    val readAddress = in UInt (log2Up(G) + w bits)
    val readRegs = in Vec(Bits(1 << w bits), G)

    val readData = out Bool()
  }

  val addressSlice = Vec(io.readAddress.subdivideIn(5 bits, false).zipWithIndex.map(a => Delay(a._1, a._2)))
  val dataArray = new Array[Vec[Bits]](addressSlice.length + 1)
  dataArray(0) = Vec(io.readRegs ++ Vec(B(0, 1 << w bits), (1 << log2Up(G)) - G))

  for (i <- addressSlice.indices) {
    dataArray(i + 1) = Reg(if (dataArray(i).getBitsWidth >> addressSlice(i).getBitsWidth <= (1 << log2Up(G)))
      Vec(Bits(1 bits), dataArray(i).getBitsWidth >> addressSlice(i).getBitsWidth) else
      Vec(Bits(dataArray(i).head.getBitsWidth >> addressSlice(i).getBitsWidth bits), 1 << log2Up(G)))
    val length = dataArray(i).length / dataArray(i + 1).length
    for (j <- dataArray(i + 1).indices) {
      dataArray(i + 1)(j) := Vec(dataArray(i).slice(j * length, (j + 1) * length))(addressSlice(i)(addressSlice(i).getBitsWidth - 1 downto addressSlice(i).getBitsWidth - log2Up(length))).subdivideIn(dataArray(i + 1)(j).getBitsWidth slices).map(_ (addressSlice(i)(addressSlice(i).getBitsWidth - log2Up(length) - 1 downto 0)).asBits).reduce(_ ## _).reversed
    }
  }

  io.readData := dataArray.last.head.lsb

  def latency: Int = addressSlice.length
}