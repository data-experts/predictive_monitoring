package de.huberlin.informatik.promi

import java.awt.Color

import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer

import com.typesafe.scalalogging.LazyLogging

import org.jfree.chart.axis.NumberAxis
import org.jfree.chart.axis.NumberTickUnit
import java.awt.Font
import org.jfree.ui.RectangleEdge
import java.awt.BasicStroke
import scalax.chart.XYChart
import weka.filters.supervised.instance.StratifiedRemoveFolds
import weka.filters.MultiFilter
import weka.filters.supervised.instance.Resample
import de.huberlin.informatik.promi.util.CachedLogParser

/**
 * Draws learning curves for a particular combination<p>
 * A learning curve contrasts the performance on the training and validation set 
 * with a varying number of training instances.<br>
 * By comparing the plots, one can investigate if an algorithm suffers from high variance / bias 
 */
class LearningCurve(config: Configuration, title : String) 
  extends scalax.chart.module.Charting with LazyLogging {

  val resolution = (1024, 468)

  def draw(steps: Seq[Double] = Seq(0.05, 0.25, 0.5, 0.75, 1.0)): Unit = {
    val trainingSeries = new XYSeries("Training F1-Score")
    val validSeries = new XYSeries("Validation F1-Score")
    val trainingSeriesROC = new XYSeries("Training Area ROC")
    val validSeriesROC = new XYSeries("Validation Area ROC")

    
    logger.info("Drawing learning curves for:\n" + config)

    val chart = XYLineChart(
        Seq(trainingSeries, validSeries, trainingSeriesROC, validSeriesROC))
    
    for (step <- steps) {
      //Abuse this filter to create Stratified subsets of training set
      val sampler = new Resample
      sampler.setSampleSizePercent(step * 100)
      val filter = new MultiFilter
      filter.setFilters(Array(config.wekaFilter, sampler))
      val results = new Experiment(config.copy(filters = Seq(() => filter), kFoldCV = None)).run()

      val noTrainingInstances = results.training.get.noTestInstances
      
      logger.info("Finished step with " + noTrainingInstances.toInt + " instances")
      Experiment.printResults(results, config)

      if (step == steps(0)) {
        val width = (noTrainingInstances / step * 1.1).toInt
        setupXYChart(chart, width, (width / 5) / 100 * 100)
        chart.show(resolution = resolution)
      }
      
      swing.Swing onEDT {
        trainingSeries.add(noTrainingInstances, results.training.get.fScore)
        validSeries.add(noTrainingInstances, results.validation.get.fScore)
        trainingSeriesROC.add(noTrainingInstances, results.training.get.areaUnderROC)
        validSeriesROC.add(noTrainingInstances, results.validation.get.areaUnderROC)
      }
    }
    logger.info("Done!")
    chart.saveAsPNG("images/" + title + "_learning_curve.png", resolution)
  }
  
  def setupXYChart(chart: XYChart, width: Double, stepWidth : Double) = {
    chart.labelGenerator = XYLabelGenerator((v: Number) => v.formatted("%.2f"))

    val font1 = new Font("Dialog", Font.PLAIN, 24)
    val font2 = new Font("Dialog", Font.PLAIN, 20)

    chart.peer.getLegend.setItemFont(font1)
    chart.peer.getLegend.setPosition(RectangleEdge.RIGHT)

    val plot = chart.plot
    plot.getRenderer.setBaseItemLabelFont(font2)

    plot.getRenderer.setSeriesStroke(0, new BasicStroke(3.0f))
    plot.getRenderer.setSeriesStroke(1, new BasicStroke(3.0f))
    plot.setBackgroundPaint(Color.WHITE)
    plot.getDomainAxis.setRange(0, width)
    plot.getDomainAxis.setLabel("No. instances")
    plot.getDomainAxis.asInstanceOf[NumberAxis].setTickUnit(new NumberTickUnit(stepWidth));
    plot.getDomainAxis.setTickLabelFont(font1)
    plot.getDomainAxis.setLabelFont(font1)
    plot.getRangeAxis.setRange(0, 1)
    plot.getRangeAxis.asInstanceOf[NumberAxis].setTickUnit(new NumberTickUnit(0.1));
    plot.getRangeAxis.setTickLabelFont(font1)
    plot.getRangeAxis.setLabelFont(font1)
  }
  
}