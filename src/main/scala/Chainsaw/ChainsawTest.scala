package Chainsaw

import Chainsaw.phases.IoAlign
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim.SimThread

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

case class ChainsawTest(
    testName: String = "testTemp",
    gen: ChainsawBaseGenerator,
    stimulus: Option[Seq[TestCase]]      = None,
    golden: Option[Seq[Seq[BigDecimal]]] = None,
    terminateAfter: Int                  = 10000,
    errorSegmentsShown: Int              = 10
) {

  import gen._

  def simConfig = {
    val spinalConfig = ChainsawSpinalConfig(gen)
    val ret = SimConfig
      .workspaceName(testName)
      .withFstWave
      .withConfig(spinalConfig)
    ret._backend = gen.simBackEnd
    ret
  }

  /** -------- get input segments
    * --------
    */
  val inputSegments: Seq[TestCase] = {
    val raw = stimulus.getOrElse(testCases)
    gen match {
      case frame: Frame =>
        raw.map { case TestCase(seg, control) =>
          val padded = frame.inputFrameFormat(control).fromRawToFrame(seg)
          TestCase(padded, control)
        }
      case _ => raw
    }
  }.sortBy(_.control.headOption.getOrElse(BigDecimal(0)))

  if (gen.isInstanceOf[SemiInfinite] && inputSegments.forall(_.data.length == inputSegments.head.data.length))
    logger.warn("all testCases have a same length, which may miss the problem in reset logic")

  if (inPortWidth != 0) {
    val pass = gen match { // check data length
      case frame: Frame =>
        inputSegments.forall { case TestCase(data, control) =>
          data.length == frame.inputFrameFormat(control).allDataCount
        }
      case _: Operator => inputSegments.map(_.data).forall(_.length == inPortWidth)
      case _           => true
    }
    require(pass, "input data length is not correct")
  }

  val targetSegmentCount = inputSegments.length
  val targetVectorCount  = inputSegments.map(_.data.length).sum / (if (inPortWidth == 0) 1 else inPortWidth)

  /** -------- interrupts insertion
    * --------
    */
  val valids = inputSegments.map(testCase => (testCase, true))

  def getRandomControl: Seq[BigDecimal] = gen match {
    case dynamic: Dynamic => dynamic.randomControlVector
    case _                => Seq[BigDecimal]()
  }

  def getRandomInterrupt = TestCase(Seq.fill(Random.nextInt(10) + 1)(randomDataVector).flatten, getRandomControl)

  def getResetInterrupt = TestCase(Seq.fill(resetCycle max 1)(randomDataVector).flatten, getRandomControl)

  val randomRatio              = 10
  val inputSegmentsWithInvalid = ArrayBuffer[(TestCase, Boolean)](valids.head)
  valids.tail.foreach { case (testCase, valid) =>
    if (!testCase.control.equals(inputSegmentsWithInvalid.last._1.control) || gen.isInstanceOf[SemiInfinite]) {
      inputSegmentsWithInvalid += ((getResetInterrupt, false))
    }
    if (Random.nextInt(randomRatio) > randomRatio - 2) {
      inputSegmentsWithInvalid += ((getRandomInterrupt, false))
    } // probability = 1/ratio
    inputSegmentsWithInvalid += ((testCase, valid))
  }
  inputSegmentsWithInvalid.insert(0, (getResetInterrupt, false)) // for initialization

  val sortedValids = inputSegmentsWithInvalid.filter(_._2).map(_._1)

  /** -------- get golden segments
    * --------
    */
  val goldenSegments: Seq[Seq[BigDecimal]] = {
    val raw = golden.getOrElse(sortedValids.map(impl))
    gen match {
      case frame: Frame =>
        raw.zip(sortedValids).map { case (seg, TestCase(_, control)) =>
          frame.outputFrameFormat(control).fromRawToFrame(seg)
        }
      case _ => raw
    }
  }

  val latencies = gen match {
    case overwriteLatency: OverwriteLatency => sortedValids.map(_ => -1)
    case fixedLatency: FixedLatency         => sortedValids.map(_ => fixedLatency.latency())
    case dynamicLatency: DynamicLatency     => sortedValids.map(testCase => dynamicLatency.latency(testCase.control))
  }

  val pass = gen match { // check golden length
    case frame: Frame =>
      val validOutputFrameLengths = sortedValids.map(testCase => frame.outputFrameFormat(testCase.control).rawDataCount)
      goldenSegments.map(_.length).equals(validOutputFrameLengths)
    case _: Operator =>
      goldenSegments.forall(_.length == outPortWidth)
    case _ => true
  }
  require(pass, "golden data length is not correct")

  require(goldenSegments.length == targetSegmentCount)

  // segments -> vectors
  val inputVectorSize = if (inPortWidth == 0) 1 else inPortWidth // when inPortWidth is 0, the module is driven by clock
  val inputVectorsWithValid: mutable.Seq[(Seq[BigDecimal], Seq[BigDecimal], Boolean, Boolean)] =
    inputSegmentsWithInvalid.flatMap { case (TestCase(segment, control), valid) =>
      val dataVectors = segment.grouped(inputVectorSize).toSeq
      gen match {
        case frame: Frame => dataVectors.init.map((_, control, valid, false)) :+ (dataVectors.last, control, valid, true)
        case _            => dataVectors.map((_, control, valid, true))
      }
    }

  // data containers
  val outputVectors = ArrayBuffer[Seq[BigDecimal]]()
  val inputTimes    = ArrayBuffer[Long]()
  val outputTimes   = ArrayBuffer[Long]()

  /** -------- simulation
    * --------
    */

  logger.info(s"current naive list: ${naiveSet.mkString(" ")}")
  simConfig.compile(gen.getImplH).doSim { dut =>
    import dut.{clockDomain, flowInPointer, flowOutPointer}

    // init
    def init(): Unit = {
      flowInPointer.valid #= false
      flowInPointer.last  #= false
      clockDomain.forkStimulus(2)
      clockDomain.waitSampling()
    }

    def poke(): SimThread = fork {
      var i = 0
      while (true) {
        if (i < inputVectorsWithValid.length) {
          val (data, control, valid, last) = inputVectorsWithValid(i)
          flowInPointer.payload.zip(data).foreach { case (fix, decimal) => fix #= decimal }
          dut match {
            case dynamicModule: DynamicModule => dynamicModule.controlIn.zip(control).foreach { case (fix, decimal) => fix #= decimal }
            case _                            => // no control
          }
          flowInPointer.valid #= valid
          flowInPointer.last  #= last
          if (valid) inputTimes += simTime()
        } else {
          flowInPointer.valid #= false
          flowInPointer.last  #= false
        }
        i += 1
        clockDomain.waitSampling()
      }
    }

    def peek(): SimThread = fork {
      while (true) {
        if (flowOutPointer.valid.toBoolean) {
          outputVectors += flowOutPointer.payload.map(_.toBigDecimal)
          outputTimes += simTime()
        }
        clockDomain.waitSampling()
      }
    }

    var currentVectors = 0
    var noValid        = 0

    // working in the main thread, pushing the simulation forward
    def waitSimDone(): Unit =
      while (currentVectors < targetVectorCount && noValid < terminateAfter) {
        if (outputVectors.length == currentVectors) noValid += 100
        currentVectors = outputVectors.length
        clockDomain.waitSampling(100)
      }

    init()
    peek()
    poke()
    waitSimDone()

    if (noValid >= terminateAfter) logger.error(s"Simulation terminated after $terminateAfter cycles of no valid output")
  }

  /** -------- check & show
    * --------
    */
  // vectors -> segments

  var outputSegments     = Seq[Seq[BigDecimal]]()
  var inputSegmentTimes  = Seq[Long]()
  var outputSegmentTimes = Seq[Long]()

  gen match {
    case frame: Frame =>
      val outputCycleCounts = sortedValids.map(testCase => frame.outputFrameFormat(testCase.control).period)
      val slices            = outputCycleCounts.scan(0)(_ + _).prevAndNext { case (start, end) => start until end }
      outputSegments     = slices.map(slice => outputVectors.slice(slice.start, slice.end)).map(_.flatten)
      inputSegmentTimes  = slices.map(slice => inputTimes.slice(slice.start, slice.end)).map(_.head)
      outputSegmentTimes = slices.map(slice => outputTimes.slice(slice.start, slice.end)).map(_.head)
    case _: SemiInfinite =>
      val discontinuities = outputTimes
        .sliding(2)
        .map { case Seq(prev, next) => if (next - prev == 2) 0 else next }
        .filterNot(_ == 0)
        .map(outputTimes.indexOf)
        .toSeq
      val splitPoints = 0 +: discontinuities :+ outputVectors.length
      outputSegments = splitPoints
        .sliding(2)
        .map { case Seq(start, end) => outputVectors.slice(start, end) }
        .toSeq
        .map(_.flatten)
      inputSegmentTimes  = splitPoints.init.map(inputTimes)
      outputSegmentTimes = splitPoints.init.map(outputTimes)
    case _: Operator =>
      outputSegments     = outputVectors
      inputSegmentTimes  = inputTimes
      outputSegmentTimes = outputTimes
  }

  require(
    outputSegments.length == goldenSegments.length,
    s"output segment count ${outputSegments.length} is not equal to golden segment count ${goldenSegments.length}"
  )

  val actualLatencies = outputSegmentTimes.zip(inputSegmentTimes).map { case (outputTime, inputTime) => (outputTime - 2 - inputTime) / 2 }

  val passRecord = inputSegments.indices.map { i =>
    val payloadPass = metric(outputSegments(i), goldenSegments(i))
    val latencyPass = actualLatencies(i) == latencies(i) || latencies(i) == -1
    payloadPass && latencyPass
  }

  val success = passRecord.forall(_ == true)

  def log(index: Int) = {
    s"-----$index-th segment-----\n" +
      s"${sortedValids(index)}\n" +
      s"yours  : ${outputSegments(index).take(500).mkString(",")}\n" +
      s"golden : ${goldenSegments(index).take(500).mkString(",")}\n" +
      s"expected latency: ${if (latencies(index) < 0) "undetermined" else latencies(index).toString}, actual latency: ${actualLatencies(index)}\n"
  }

  val logIndices = if (success) Seq(outputSegments.length - 1) else passRecord.zipWithIndex.filter(!_._1).map(_._2)
  val allLog     = logIndices.take(errorSegmentsShown).map(log).mkString("\n")

  if (!success) logger.error(s"failures:\n$allLog")
  else logger.info(s"test $testName passed\n$allLog")
  assert(success)
}

object ChainsawTestWithData {
  def apply(testName: String, gen: ChainsawBaseGenerator, data: Seq[TestCase], golden: Seq[Seq[BigDecimal]]) =
    ChainsawTest(testName, gen, Some(data), Some(golden))
}
