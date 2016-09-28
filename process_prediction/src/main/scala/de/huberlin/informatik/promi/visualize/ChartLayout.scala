package de.huberlin.informatik.promi.visualize

import java.awt.Font
import scalax.chart.Chart
import org.jfree.ui.RectangleEdge
import java.awt.Color
import org.jfree.chart.plot.XYPlot

trait ChartLayout {

  def font1: Font
  def font2: Font
  def fontBold : Font
  def position : RectangleEdge

  def layoutChart(chart: Chart): Unit = {
    chart.peer.getLegend.setItemFont(font1)
    chart.peer.getLegend.setPosition(position)

    chart.peer.getTitle.setFont(fontBold)
    
    val plot = chart.plot.asInstanceOf[XYPlot]
    plot.getRenderer.setBaseItemLabelFont(font2)

    plot.setBackgroundPaint(Color.WHITE)
    plot.getDomainAxis.setTickLabelFont(font1)
    plot.getDomainAxis.setLabelFont(font1)
    plot.getRangeAxis.setTickLabelFont(font1)
    plot.getRangeAxis.setLabelFont(font1)
  }
}

object PresentationLayout extends ChartLayout {
  val font1 = new Font("Dialog", Font.PLAIN, 30)
  val font2 = new Font("Dialog", Font.PLAIN, 24)
  val fontBold = new Font("Dialog", Font.BOLD, 30)
  val position = RectangleEdge.BOTTOM
}

object PaperLayout extends ChartLayout {
  val font1 = new Font("Dialog", Font.PLAIN, 24)
  val font2 = new Font("Dialog", Font.PLAIN, 20)
  val fontBold = new Font("Dialog", Font.BOLD, 24)
   val position = RectangleEdge.RIGHT
}