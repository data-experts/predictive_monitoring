package de.huberlin.informatik.promi

import de.huberlin.informatik.promi.map._

import org.deckfour.xes.model.XTrace
import org.deckfour.xes.model.XLog
import org.deckfour.xes.in.XesXmlParser

import de.huberlin.informatik.promi.visualize.Table

import scala.collection.JavaConversions._
import weka.core._
import weka.filters.{ Filter => WekaFilter }
import weka.filters.unsupervised.attribute.Standardize

import weka.classifiers.Evaluation
import com.typesafe.scalalogging.LazyLogging
import weka.classifiers.evaluation.ThresholdCurve
import weka.gui.visualize._
import java.awt._
import geom._
import scalax.chart.Chart
import org.jfree.util.ShapeUtilities
import org.jfree.chart.renderer.xy.XYLineAndShapeRenderer
import org.jfree.chart.plot.Plot

/**
 * Single run of an experiment configured by a Configuration object<p>
 *
 * Contains various helper methods for outputting experimental results
 */
object Experiment extends LazyLogging {

  def printResults(results: Results, config: Configuration, name: String = "", debug: Boolean = false) = {
    def mkTable(title: String, result: Result) = {
      Table.printTable(title, at => {
        at.addRow("Accuracy", result.accuracy.toString)
        at.addRow("F-Score", result.fScore.toString)
        at.addRow("Precision", result.precision.toString)
        at.addRow("Recall", result.recall.toString)
        at.addRow("Area under ROC", result.areaUnderROC.toString)
        at.addRow("TP", result.truePositives.toString)
        at.addRow("FP", result.falsePositives.toString)
        at.addRow("TN", result.trueNegatives.toString)
        at.addRow("FN", result.falseNegatives.toString)
      }, 46, debug = debug)
    }
    results match {
      case Results(trainResult, validResult, cvResult, testResult) =>
        trainResult.foreach(mkTable(name + " performance on Training Data", _))
        cvResult.foreach(mkTable(name + s" performance on ${config.kFoldCV.getOrElse("?")}-fold Cross Validation", _))
        validResult.foreach(mkTable(name + " performance on Validation Set", _))
        testResult.foreach(mkTable(name + " performance on Test Set", _))
    }
  }

  case class Result(
    accuracy: Double,
    fScore: Double,
    precision: Double,
    recall: Double,
    weightedFScore: Double,
    truePositives: Double,
    falsePositives: Double,
    trueNegatives: Double,
    falseNegatives: Double,
    rocCurve: Instances,
    areaUnderROC: Double,
    noTestInstances: Double)

  object Result {

    def fromEvaluation(eval: Evaluation, idx: Int = 1): Some[Result] =
      Some(Result(
        1 - eval.errorRate(),
        eval.fMeasure(idx),
        eval.precision(idx),
        eval.recall(idx),
        eval.weightedFMeasure(),
        eval.numTruePositives(idx),
        eval.numFalsePositives(idx),
        eval.numTrueNegatives(idx),
        eval.numFalseNegatives(idx),
        new ThresholdCurve().getCurve(eval.predictions(), idx),
        eval.areaUnderROC(idx),
        eval.numInstances()))
  }

  case class Results(
    training: scala.Option[Result],
    validation: scala.Option[Result],
    crossValidation: scala.Option[Result],
    test: scala.Option[Result])

}

import Experiment._
import java.awt.geom.Rectangle2D
import weka.classifiers.evaluation.NominalPrediction
import weka.classifiers.meta.ThresholdSelector
import weka.filters.Filter
import weka.filters.AllFilter
import weka.filters.unsupervised.instance.RemoveWithValues
import de.huberlin.informatik.promi.experiments.EvaluationSetting

import scalax.chart.XYChart
import de.huberlin.informatik.promi.util._
import de.huberlin.informatik.promi.visualize.Table
import de.huberlin.informatik.promi.visualize.Table
import de.huberlin.informatik.promi.visualize.Table
import weka.classifiers.SingleClassifierEnhancer
import weka.classifiers.meta.FilteredClassifier
import weka.classifiers.evaluation.output.prediction.PlainText

class Experiment(val baseConfig: Configuration) extends LazyLogging {

  def run(): Results = {
    baseConfig.output match {
      case OneClassUnsupervised => runOutlierDetectionWithoutTraining()
      case _ => runBinaryClassification()
    }
  }

  def runBinaryClassification(): Results = {
    val (train, valid, test) = Splitter.split(baseConfig)

    val (cvResult, newConfig) = optimizeParameters(train)

    val (trainingSet, schema, filter) = WekaConverter.getInstances(train, newConfig, "Training")

    val validSet = WekaConverter.getInstances(valid, newConfig, "Validation", schema, filter)

    val noTrainingInstances = trainingSet.numInstances()

    logger.trace(trainingSet.firstInstance.toStringNoWeight)
    logger.trace(trainingSet.firstInstance.classValue().toString())
    logger.trace(trainingSet.toSummaryString())
    logger.info("Class distribution for training set")
    logger.info(trainingSet.attributeStats(trainingSet.numAttributes() - 1).toString())

    val classifier = newConfig.classifier.get.wekaClassifier(newConfig, schema)

    classifier.buildClassifier(trainingSet)

    logger.trace(classifier.toString())

    val validResult = if (validSet.numInstances() == 0) { None } else {
      logger.info("Class distribution for validation set")
      logger.info(validSet.attributeStats(validSet.numAttributes() - 1).toString())

      val validEval = new Evaluation(validSet)
      validEval.evaluateModel(classifier, validSet)
      val classIndex = baseConfig.classIndex
      val validResult = Result.fromEvaluation(validEval, classIndex)

      printPredictions(classifier, newConfig, validSet, validEval)
      validResult
    }

    // Evaluate on training set
    val trainingEval = new Evaluation(trainingSet)
    trainingEval.evaluateModel(classifier, trainingSet)
    val trainingResult = Result.fromEvaluation(trainingEval, baseConfig.classIndex)

    val testSet = WekaConverter.getInstances(test, newConfig, "Testset", schema, filter)

    val testResult = if (!newConfig.useTestSet || testSet.numInstances() == 0) { None } else {
      logger.info("Class distribution for test set")
      logger.info(testSet.attributeStats(testSet.numAttributes() - 1).toString())

      val testEval = new Evaluation(testSet)
      val classIndex = baseConfig.classIndex
      val collector = new PredictionCollector(testSet, baseConfig)
      testEval.evaluateModel(classifier, testSet, collector)
      collector.collect("TEST set")
      Result.fromEvaluation(testEval, classIndex)
    }

    Results(trainingResult, validResult, cvResult, testResult)
  }

  def runOutlierDetectionWithoutTraining(): Results = {
    val (train, valid, test) = Splitter.split(baseConfig)

    val (cvResult, newConfig) = optimizeParameters(train)
    val (testSet, schema, filter) = WekaConverter.getInstances(test, newConfig, "Test")

    val classifier = newConfig.classifier.get.wekaClassifier(newConfig, schema)

    classifier.buildClassifier(testSet)

    logger.trace(classifier.toString())

    val testResult = if (testSet.numInstances() == 0) { None } else {
      logger.info("Class distribution for test set")
      logger.info(testSet.attributeStats(testSet.numAttributes() - 1).toString())

      val testEval = new Evaluation(testSet)
      val classIndex = baseConfig.classIndex
      val collector = new PredictionCollector(testSet, baseConfig)
      testEval.evaluateModel(classifier, testSet, collector)
      val testResult = collector.collect("TEST set")
      Some(testResult)
    }

    Results(None, None, cvResult, testResult)
  }

  def optimizeParameters(trainingLog: XLog): (scala.Option[Result], Configuration) = {
    baseConfig.gridSearch match {
      case None => // Do not perform Gridsearch and return CV result and base config
        val res: scala.Option[Result] = baseConfig.kFoldCV.map(k => {
          logger.warn("No model selection is performed")
          val (trainingSet, schema, filter) = WekaConverter.getInstances(trainingLog, baseConfig, "Training")
          logger.info(s"Running $k fold cross-validation")
          val collector = new PredictionCollector(trainingSet, baseConfig)
          val cvEval = new Evaluation(trainingSet)
          cvEval.crossValidateModel(
            baseConfig.classifier.get.wekaClassifier(baseConfig, trainingSet),
            trainingSet, k, new java.util.Random(), collector)
          Result.fromEvaluation(cvEval, baseConfig.classIndex).get
        })
        (res, baseConfig)
      case Some(gridSearch) =>
        logger.info("Running Gridsearch for model selection")
        val GridSearchResult(config, params, res, eval) = gridSearch.run(trainingLog, baseConfig)
        logger.info("Found params via cross validation: " + params)
        (Some(res), config)
    }
  }

  def unWrap(classifier: weka.classifiers.Classifier): weka.classifiers.Classifier = {
    classifier match {
      case t: ThresholdSelector => unWrap(t.getClassifier)
      case _ => classifier
    }
  }

  def printPredictions(
    classifier: weka.classifiers.Classifier,
    config: Configuration,
    dataset: Instances,
    evaluation: Evaluation): Unit = {
    val classIndex = config.classIndex
    val actualClassy = unWrap(classifier)

    Table.printTable("List of predictions", table => {
      table.addRow("Year", "Applicant", "Dep.", "Prob.", "(Actual) Prob.", "Prediction", "Actual", "Correct?")
      dataset.zip(evaluation.predictions()).sortWith {
        case ((_, p1: NominalPrediction), (_, p2: NominalPrediction)) =>
          if (config.output.isOneClass)
            p1.distribution()(classIndex) < p2.distribution()(classIndex)
          else
            p1.distribution()(classIndex) > p2.distribution()(classIndex)
      }.foreach {
        case (inst, p: NominalPrediction) => {
          def attrToString(name: String) = {
            val att = dataset.attribute(name)
            if (att == null) "" else inst.toString(att)
          }
          table.addRow(
            attrToString("year"),
            attrToString("applicant"),
            attrToString("dep"),
            "%f" format p.distribution()(classIndex),
            "%f" format actualClassy.distributionForInstance(inst)(classIndex),
            "%.0f" format p.predicted(),
            "%.0f" format p.actual(),
            if (p.predicted() == p.actual()) "YES" else "NO")
        }
      }

    }, trace = true, width = 120)
  }

}
