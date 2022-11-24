package Chainsaw.crypto

import Chainsaw._
import Chainsaw.xilinx._
import Chainsaw.arithmetic._
import Chainsaw.dag._
import Chainsaw.project.zprize.ZPrizeMSM.baseModulus
import cc.redberry.rings.scaladsl._
import spinal.core.SpinalVerilog

import java.awt.Color
import java.io.File
import javax.imageio.ImageIO
import scala.collection.mutable.ArrayBuffer

case class EllipticCurveAdder(ecCurve: EllipticCurve, width: Int, constantModulus: Option[BigInt])
  extends Dag {

  val X1, Y1, Z1, T1, X2, Y2, Z2, T2 = InputVertex(UIntInfo(width))
  val X, Y, Z, T = OutputVertex(UIntInfo(width))

  X1.vertex.setName("X1")
  Y1.vertex.setName("Y1")
  Z1.vertex.setName("Z1")
  T1.vertex.setName("T1")
  X2.vertex.setName("X2")
  Y2.vertex.setName("Y2")
  Z2.vertex.setName("Z2")
  T2.vertex.setName("T2")
  X.vertex.setName("X")
  Y.vertex.setName("Y")
  Z.vertex.setName("Z")
  T.vertex.setName("T")

  val r0Mul = Barrett(width, constantModulus, FullMultiplier).asVertex
  val r1CMul = Barrett(width, constantModulus, FullMultiplier).asVertex
  val k = ConstantVertex(UIntInfo(width), BigInt("0196bab03169a4f2ca0b7670ae65fc7437786998c1a32d217f165b2fe0b32139735d947870e3d3e4e02c125684d6e016", 16))

  val R1Sub = ModularAdd(width, constantModulus, true).asVertex
  val R2Sub = ModularAdd(width, constantModulus, true).asVertex
  val R3Add = ModularAdd(width, constantModulus, false).asVertex
  val R4Add = ModularAdd(width, constantModulus, false).asVertex

  val R5Mul = Barrett(width, constantModulus, FullMultiplier).asVertex
  val R6Mul = Barrett(width, constantModulus, FullMultiplier).asVertex
  val R7Mul = Barrett(width, constantModulus, FullMultiplier).asVertex
  val R8CMul = ModularAdd(width, constantModulus, false).asVertex

  val R9Sub = ModularAdd(width, constantModulus, true).asVertex
  val R10Sub = ModularAdd(width, constantModulus, true).asVertex
  val R11Add = ModularAdd(width, constantModulus, false).asVertex
  val R12Add = ModularAdd(width, constantModulus, false).asVertex

  val XMul = Barrett(width, constantModulus, FullMultiplier).asVertex
  val YMul = Barrett(width, constantModulus, FullMultiplier).asVertex
  val ZMul = Barrett(width, constantModulus, FullMultiplier).asVertex
  val TMul = Barrett(width, constantModulus, FullMultiplier).asVertex

  r0Mul := (Z1, Z2)
  r1CMul := (T2, k)

  R1Sub := (Y1, X1)
  R2Sub := (Y2, X2)
  R3Add := (Y1, X1)
  R4Add := (Y2, X2)

  R5Mul := (R1Sub.out(0), R2Sub.out(0))
  R6Mul := (R3Add.out(0), R4Add.out(0))
  R7Mul := (T1, r1CMul.out(0))
  R8CMul := (r0Mul.out(0), r0Mul.out(0))

  R9Sub := (R6Mul.out(0), R5Mul.out(0))
  R10Sub := (R8CMul.out(0), R7Mul.out(0))
  R11Add := (R8CMul.out(0), R7Mul.out(0))
  R12Add := (R6Mul.out(0), R5Mul.out(0))

  XMul := (R9Sub.out(0), R10Sub.out(0))
  YMul := (R11Add.out(0), R12Add.out(0))
  ZMul := (R10Sub.out(0), R11Add.out(0))
  TMul := (R9Sub.out(0), R12Add.out(0))

  X := XMul.out(0)
  Y := YMul.out(0)
  Z := ZMul.out(0)
  T := TMul.out(0)

  graphDone()

  override def name: String = "padd"

  /** --------
   * golden model
   * -------- */
  override def impl(dataIn: Seq[Any]): Seq[Any] = {
    val data = dataIn.asInstanceOf[Seq[BigInt]]
    ecCurve.asInstanceOf[ShortWeierstrassCurve].paddExtendedHomo(EcPointProj(data.slice(0, 4).map(asBigInteger): _*)(extendedHomo), EcPointProj(data.slice(4, 8).map(asBigInteger): _*)(extendedHomo)).axis.map(_.toBigInt)
  }
}

object EllipticCurveAdder extends App {
  val curve = new ShortWeierstrassCurve(baseModulus, 0, 1)
  val graph = EllipticCurveAdder(curve, 377, Option(baseModulus))
  toPng(graph, "padd")
  SpinalVerilog(graph.implH)

  def toPng(dag: Dag, pngName: String = null): Unit = {

    implicit val refDag: Dag = dag

    type V = DagVertex
    type E = DagEdge
    type Port = DagPort

    import _root_.com.mxgraph.layout._
    import _root_.com.mxgraph.util.mxCellRenderer
    import org.jgrapht.ext.JGraphXAdapter

    val graphAdapter = new JGraphXAdapter[V, E](dag) // manager which store the information for rendering

    val vertexMap = graphAdapter.getVertexToCellMap
    val edgeMap = graphAdapter.getEdgeToCellMap

    // set styles of vertices/edges
    def colorVertices(vertices: Seq[V], color: String): Array[AnyRef] = {
      val targets = vertices.map(vertexMap.get(_)).toArray
      graphAdapter.setCellStyle(s"fillColor=#$color", targets.asInstanceOf[Array[Object]])
    }

    def colorEdges(edges: Seq[E], color: String): Array[AnyRef] = {
      val targets = edges.map(edgeMap.get(_)).toArray
      graphAdapter.setCellStyle(s"strokeColor=#$color", targets.asInstanceOf[Array[Object]])
    }

    /** --------
     * coloring
     * -------- */
    val postVertices = ArrayBuffer[V]()
    val postEdges = ArrayBuffer[E]()

    var currents: Seq[E] = dag.outputs.flatMap(_.incomingEdges)
    while (currents.nonEmpty) {
      val drivingVertices = currents.map(_.source).filter(v => v.isIo)
      val drivingEdges = drivingVertices.flatMap(_.incomingEdges)
      postEdges ++= drivingEdges
      postVertices ++= drivingVertices
      currents = drivingEdges
    }

    colorVertices(postVertices, "CCCC00")
    colorEdges(postEdges, "CCCC00")

    /** --------
     * constructing layout
     * -------- */

    var vertexNow = dag.outputs
    var level = 0

    while (vertexNow.nonEmpty) {
      val newVertex = new ArrayBuffer[DagVertex]

      for (i <- vertexNow.indices) {
        val g = vertexMap.get(vertexNow(i)).getGeometry
        g.setRect(200 * i, 100 * level, g.getWidth, g.getHeight)

        val edges = dag.incomingEdgesOf(vertexNow(i))
        val edgesIter = edges.iterator()
        while (edgesIter.hasNext) {
          val verTex = dag.getEdgeSource(edgesIter.next)
          if (!newVertex.contains(verTex)) {
            newVertex += verTex
          }
        }
      }

      vertexNow = newVertex
      level += 1
    }

    // using the mxGraph built-in CompactTreeLayout as our beginning
    //    val layout = new mxCompactTreeLayout(graphAdapter, true, true)
    //    // settings
    //    layout.setMoveTree(true)
    //    layout.setEdgeRouting(false)
    //    layout.setResizeParent(false)
    //    layout.setLevelDistance(5)
    //
    //    layout.execute(graphAdapter.getDefaultParent) // starts from a tree layout

    // customization, according to the pipeline information
    // TODO: better layout algo

    //    def doLayoutByRetimingInfo(): Unit = {
    //      val timeMax = dag.retimingInfo.values.max
    //      val initialView = layout.getGraph.getView
    //
    //      def adjustY(v: V, y: Double) = {
    //        val cell = vertexMap.get(v)
    //        layout.setVertexLocation(cell, initialView.getState(cell).getX, y)
    //      }
    //
    //      val pipelineGap = 50
    //      dag.inputs.zipWithIndex.foreach { case (v, _) => adjustY(v, 0) }
    //      dag.outputs.zipWithIndex.foreach { case (v, _) => adjustY(v, (timeMax + 2) * pipelineGap) }
    //
    //      dag.retimingInfo
    //        .filterNot(_._1.isIo)
    //        .groupBy(_._2).foreach { case (_, vToInt) => vToInt.foreach { case (v, i) => adjustY(v, (i + 1) * pipelineGap) }
    //      }
    //    }

    //    doLayoutByRetimingInfo

    /** --------
     * png generation
     * -------- */
    val image = mxCellRenderer.createBufferedImage(graphAdapter, null, 2, Color.WHITE, true, null)
    val fileName = if (pngName == null) dag.name else pngName
    val imgFile = new File(dagFigDir, s"$fileName.png")
    logger.info(s"view png generated for ${dag.name} in \n${imgFile.getAbsoluteFile}")
    ImageIO.write(image, "PNG", imgFile)
  }
}
