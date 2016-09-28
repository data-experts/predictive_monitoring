package de.huberlin.informatik.promi.visualize

import org.jfree.util.ShapeUtilities
import java.awt.BorderLayout
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer
import weka.gui.visualize.ThresholdVisualizePanel
import weka.classifiers.evaluation.ThresholdCurve
import weka.core.Instances
import weka.gui.visualize.PlotData2D
import java.awt.geom.Rectangle2D
import java.awt.Color
import weka.core.Utils
import com.typesafe.scalalogging.LazyLogging

object Charts extends LazyLogging with scalax.chart.module.Charting {
  
  def drawROCCurve(title: String, results: Seq[(String, Instances)]): scalax.chart.Chart = {
    val size = 3f
    val rect = new Rectangle2D.Float(-size, -size, size * 2, size * 2)
    val symbols = Seq(
      rect,
      ShapeUtilities.rotateShape(rect, Math.PI / 4 /*45 degree*/ , 0, 0),
      ShapeUtilities.createDownTriangle(size),
      ShapeUtilities.createUpTriangle(size),
      ShapeUtilities.createRegularCross(size, size),
      ShapeUtilities.createDiamond(size),
      ShapeUtilities.createDiagonalCross(size, size))
    val colors = Seq(
      /*Color.GRAY,*/ Color.BLACK, Color.BLUE, Color.CYAN, Color.GREEN, Color.RED, Color.MAGENTA)

    val curves = results.map {
      case (name, result) => {
        val tpInd = result.attribute(ThresholdCurve.FP_RATE_NAME).index();
        val fpInd = result.attribute(ThresholdCurve.TP_RATE_NAME).index();
        val tpVals = result.attributeToDoubleArray(tpInd);
        val fpVals = result.attributeToDoubleArray(fpInd);

        val curve = new XYSeries(name)

        val tickSize = if (result.numInstances() < 20) {
          logger.warn("Hmm... very few results?")
          result.numInstances()
        } else result.numInstances / 20

        for ((tp, fp) <- tpVals.zip(fpVals).grouped(tickSize).map(_.head)) {
          curve.add(tp, fp)
        }
        curve
      }
    }

    val curveRandom = new XYSeries(s"random")

    for (i <- 0.0 to 1.0 by 0.1) {
      curveRandom.add(i, i)
    }

    val chart = XYLineChart(curves :+ curveRandom)

    for (i <- 0 to curves.length) {
      val r = chart.plot.getRenderer.asInstanceOf[XYLineAndShapeRenderer]
      if (i == curves.length) { // random guess
        r.setSeriesPaint(i, Color.BLACK)
      } else {
        r.setSeriesShape(i, symbols(i))
        r.setSeriesShapesVisible(i, true);
        r.setSeriesPaint(i, colors(i))
      }
    }

    chart.plot.getDomainAxis.setLabel("FP rate")
    chart.plot.getDomainAxis.setRange(0.0, 1.05)
    chart.plot.getRangeAxis.setLabel("TP rate")
    chart.plot.setBackgroundPaint(Color.WHITE)
    chart.title = title
    chart
  }

  def drawWekaEvaluation(result: Instances) = {
    result.setClassIndex(result.numAttributes() - 1);
    val tc = new ThresholdCurve();
    // method visualize
    val vmc = new ThresholdVisualizePanel();
    vmc.setROCString("(Area under ROC = " +
      Utils.doubleToString(ThresholdCurve.getROCArea(result), 4) + ")");
    vmc.setName(result.relationName());
    val tempd = new PlotData2D(result);
    tempd.setPlotName(result.relationName());
    tempd.addInstanceNumberAttribute();
    // specify which points are connected
    val cp = new Array[Boolean](result.numInstances());
    for (n <- 1 until cp.length) cp(n) = true;
    tempd.setConnectPoints(cp);
    // add plot
    vmc.addPlot(tempd);
    // method visualizeClassifierErrors
    val plotName = vmc.getName();
    val jf =
      new javax.swing.JFrame("Weka Classifier Visualize: " + plotName);
    jf.setSize(500, 400);
    jf.getContentPane().setLayout(new BorderLayout());

    jf.getContentPane().add(vmc, BorderLayout.CENTER);
    jf.addWindowListener(new java.awt.event.WindowAdapter() {
      override def windowClosing(e: java.awt.event.WindowEvent) {
        jf.dispose();
      }
    });

    jf.setVisible(true);
  }
  
}