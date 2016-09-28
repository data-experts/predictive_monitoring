package de.huberlin.informatik.promi.experiments

import de.huberlin.informatik.promi.Configuration
import de.huberlin.informatik.promi.Experiment
import de.huberlin.informatik.promi.Experiment._
import org.jfree.chart.plot.CombinedDomainXYPlot
import org.jfree.chart.ChartFactory
import scalax.chart.XYChart
import de.huberlin.informatik.promi.visualize.Charts
import scalax.chart.module.Exporting
import de.huberlin.informatik.promi.visualize.ChartLayout
import com.typesafe.scalalogging.LazyLogging
import weka.core.Instances
import ch.qos.logback.classic.LoggerContext
import org.slf4j.LoggerFactory
import java.text.SimpleDateFormat
import java.io.File
import de.huberlin.informatik.promi.util.Splitter
import de.huberlin.informatik.promi.visualize.PaperLayout
import de.huberlin.informatik.promi.visualize.PresentationLayout

trait EvaluationSetting extends Exporting with LazyLogging {

  def title: String
  def abbrv: String
  def classyName: String
  def split: Splitter.Split
  def noiseTreeMiner : Option[Float]
  def fileNames : Seq[String]
  
  val res = (1024, 768)

  val resultFolder = new java.io.File("../results")

  def main(args: Array[String]): Unit = {
    val appName = title.replace(" ", "_").replace(".", "").toLowerCase
    val dateString = new SimpleDateFormat("YYYY-MM-dd_HH-mm-ss").format(new java.util.Date())
    val splitType = split.getClass.getSimpleName
    val folder =
      new File(
        new File(
          new File(
            new File(
              new File(
                resultFolder,
                abbrv), appName), splitType), classyName), dateString)
    System.setProperty("resultFolder", folder.getPath)
    val imgFolder = new java.io.File(folder, "images")
    if (!imgFolder.exists()) {
      imgFolder.mkdirs()
    }

    for ((name, settings) <- getConfigs()) {
      logger.info("+++++ Running: " + name + "\n")
      val results = for ((name, config) <- settings)
        yield name -> {
        val results = new Experiment(config).run
        printResults(results, config, name)
        (results.crossValidation,
          results.test)
      }
      logger.debug(
          "\t\tTrain-Pos\tTrain-Neg\tCV-F1\tCV-AUC\tTest-Pos\tTest-Neg\tTest-F1\tTest-AUC\tTest-Pr\tTest-R\tTest-TP\tTest-FP\tTest-TN\tTest-FN")
      for ((name, (Some(cv), Some(test))) <- results) {
      logger.debug(
          s"\t$name\t${cv.truePositives+cv.falseNegatives}\t${cv.falsePositives+cv.trueNegatives}\t${cv.fScore}\t${cv.areaUnderROC}\t"+
          s"${test.truePositives+test.falseNegatives}\t${test.falsePositives+test.trueNegatives}\t${test.fScore}\t${test.areaUnderROC}\t"+
          s"${test.precision}\t${test.recall}\t${test.truePositives}\t${test.falsePositives}\t${test.trueNegatives}\t${test.falseNegatives}")
      }
      plot match {
        case ROCCurve => {
          def show(prefix: String, results: Seq[(String, Instances)]) = {
            val chart = Charts.drawROCCurve(name, results)
            try {
              PaperLayout.layoutChart(chart)
              chart.saveAsPNG(
                imgFolder.getPath + "/" + "print_" + (prefix + "_" + name).replaceAll("[_. ]", "").toLowerCase + ".png", res)
              PresentationLayout.layoutChart(chart)
              chart.saveAsPNG(
                imgFolder.getPath + "/" + "pres_" + (prefix + "_" + name).replaceAll("[_. ]", "").toLowerCase + ".png", res)
            } catch {
              case e: Exception => logger.error("JFree-Chart error", e)
            }
          }
          val cv = for ((name, (Some(res), _)) <- results) yield (name, res.rocCurve)
          if (!cv.isEmpty) show("CV", cv)
          val test = for ((name, (_, Some(res))) <- results) yield (name, res.rocCurve)
          if (!test.isEmpty) show("Test", test)
        }
        case NoPlot =>
        case _ => ???
      }
    }
  }

  sealed trait Plot
  case object NoPlot extends Plot
  case object ROCCurve extends Plot

  def plot(): Plot

  def getConfigs(): Seq[(String, Seq[(String, Configuration)])]

}