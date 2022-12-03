package Chainsaw.pippenger.implement

import spinal.core._
import spinal.lib._

object decreaseCounter {
  def apply(start: BigInt, end: BigInt): decreaseCounter = new decreaseCounter(start = start, end = end)

  def apply(range: Range): decreaseCounter = {
    require(range.step == -1)
    decreaseCounter(start = range.high, end = range.low)
  }

  def apply(stateCount: BigInt): decreaseCounter = new decreaseCounter(start = stateCount - 1, end = 0)

  def apply(bitCount: BitCount): decreaseCounter = new decreaseCounter(start = (BigInt(1) << bitCount.value) - 1, end = 0)

  def apply(start: BigInt, end: BigInt, dec: Bool): decreaseCounter = {
    val counter = decreaseCounter(start, end)
    when(dec) {
      counter.decrement()
    }
    counter
  }

  def apply(range: Range, inc: Bool): Counter = {
    require(range.step == 1)
    Counter(start = range.low, end = range.high, inc = inc)
  }

  def apply(stateCount: BigInt, inc: Bool): Counter = Counter(start = 0, end = stateCount - 1, inc = inc)

  def apply(bitCount: BitCount, inc: Bool): Counter = Counter(start = 0, end = (BigInt(1) << bitCount.value) - 1, inc = inc)
}

// start and end inclusive, up counter
class decreaseCounter(val start: BigInt, val end: BigInt) extends ImplicitArea[UInt] {
  require(start >= end)
  val willDecrement = False.allowOverride
  val willClear = False.allowOverride

  def clear(): Unit = willClear := True

  def decrement(): Unit = willDecrement := True

  val valueNext = UInt(log2Up(start + 1) bit)
  val value = RegNext(valueNext) init (start)
  val willUnderflowIfDec = value === end
  val willUnderflow = willUnderflowIfDec && willDecrement

  if (isPow2(start + 1) && end == 0) { //Check if using overflow follow the spec
    valueNext := (value - U(willDecrement)).resized
  }
  else {
    when(willUnderflow) {
      valueNext := U(start)
    } otherwise {
      valueNext := (value - U(willDecrement)).resized
    }
  }
  when(willClear) {
    valueNext := start
  }

  willUnderflowIfDec.allowPruning()
  willUnderflow.allowPruning()

  override def implicitValue: UInt = this.value

  /**
   * Convert this stream to a flow. It will send each value only once. It is "start inclusive, end exclusive".
   * This means that the current value will only be sent if the counter increments.
   */
  def toFlow(): Flow[UInt] = {
    val flow = Flow(value)
    flow.payload := value
    flow.valid := willDecrement
    flow
  }
}