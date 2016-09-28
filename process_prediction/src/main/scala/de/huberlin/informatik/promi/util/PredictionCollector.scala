package de.huberlin.informatik.promi.util

import weka.classifiers.evaluation.output.prediction.AbstractOutput
import weka.core.Instance
import weka.classifiers.Classifier
import weka.classifiers.meta.ThresholdSelector
import de.huberlin.informatik.promi.visualize.Table
import de.vandermeer.asciitable.v2.V2_AsciiTable
import weka.core.Instances
import de.huberlin.informatik.promi.Configuration
import de.huberlin.informatik.promi.map.OneClass
import com.typesafe.scalalogging.LazyLogging
import de.huberlin.informatik.promi.Experiment.Result

/**
 * Helper class that can be used in conjunction with {@link weka.classifiers.Evaluation}
 * to collect a ranked list of predictions
 */
class PredictionCollector(
  val dataset: Instances,
  val config: Configuration)
    extends AbstractOutput with LazyLogging {

  setBuffer(new StringBuffer())
  setHeader(dataset)

  case class Pred(
    year: String,
    dep: String,
    applicant: String,
    prediction: Double,
    actualPrediction: Double,
    predictedClass: Double,
    actualClass: Double)

  val list = scala.collection.mutable.ListBuffer[Pred]()

  def doPrintClassification(pred: Array[Double], inst: Instance, index: Int): Unit = {
  }

  def doPrintClassification(wrappedClassy: Classifier, inst: Instance, index: Int): Unit = {
    val classy = unWrap(wrappedClassy)
    def attrToString(name: String) = {
      val att = dataset.attribute(name)
      if (att == null) "" else inst.toString(att)
    }
    val predicted = 
      if (config.output.isOneClass) 1 - wrappedClassy.distributionForInstance(inst)(config.classIndex)
      else wrappedClassy.distributionForInstance(inst)(config.classIndex)
    val actualPredicted = 
      if (config.output.isOneClass) 1 - classy.distributionForInstance(inst)(config.classIndex)
      else classy.distributionForInstance(inst)(config.classIndex)
      
    val attLabel = dataset.attribute("Label")
    val actual = inst.value(attLabel)
    list += (Pred(
      attrToString("year"),
      attrToString("applicant"),
      attrToString("dep"),
      predicted,
      actualPredicted,
      if (predicted > 0.5) 1.0 else 0.0,
      actual))
  }

  def doPrintFooter(): Unit = {
  }

  def doPrintHeader(): Unit = {
    //    table.addRow("Year", "Applicant", "Dep.", "Prob.", "(Actual) Prob.", "Prediction", "Actual", "Correct?")
  }

  def getDisplay(): String = {
    ""
  }

  def globalInfo(): String = {
    ""
  }

  private def unWrap(classifier: weka.classifiers.Classifier): weka.classifiers.Classifier = {
    classifier match {
      case t: ThresholdSelector => unWrap(t.getClassifier)
      case _ => classifier
    }
  }

  def collect(datasetName: String) : Result = {
    var tp = 0f
    var fp = 0f
    var tn = 0f
    var fn = 0f
    Table.printTable("List of predictions on " + datasetName, table => {
      table.addRow("Year", "Applicant", "Dep.", "Prob.", "(Actual) Prob.", "Prediction", "Actual", "Correct?")
      list.sortBy { _.actualPrediction }.reverseIterator.foreach { pred =>
        table.addRow(
          pred.year,
          pred.applicant,
          pred.dep,
          "%f" format pred.prediction,
          "%f" format pred.actualPrediction,
          "%.0f" format pred.predictedClass,
          "%.0f" format pred.actualClass,
          if (pred.predictedClass == pred.actualClass) "YES" else "NO")
        (pred.predictedClass, pred.actualClass) match {
          case (0,0) => tn+=1
          case (1,0) => fp+=1
          case (0,1) => fn+=1
          case (1,1) => tp+=1
        }
      }
    }, trace = true, width = 120)
  val p = tp / (tp + fp)
  val r = tp / (tp + fn)
  val f1 = 2 * p * r / (p + r)
  Result(
    accuracy = (tp + tn) / (tp + tn + fp + fn),
    fScore = f1,
    precision = p,
    recall = r,
    weightedFScore = f1 ,
    truePositives = tp,
    falsePositives = fp,
    trueNegatives = tn,
    falseNegatives = fn,
    rocCurve = null,
    areaUnderROC = 0,
    noTestInstances = tp + tn + fp + fn
  )
  }
}