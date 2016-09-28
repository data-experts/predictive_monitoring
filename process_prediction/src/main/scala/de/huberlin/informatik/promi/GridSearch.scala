package de.huberlin.informatik.promi

import de.huberlin.informatik.promi.Experiment._
import com.typesafe.scalalogging.LazyLogging
import weka.filters.AllFilter
import weka.classifiers.Evaluation
import weka.core.Instances
import de.huberlin.informatik.promi.experiments.EvaluationSetting
import org.deckfour.xes.model.XLog
import weka.filters.Filter
import de.huberlin.informatik.promi.util.WekaConverter
import de.huberlin.informatik.promi.util.PredictionCollector
import de.huberlin.informatik.promi.experiments.ByYearSplit

import scala.collection.mutable
import scala.collection.JavaConversions._
import scala.collection.mutable.ListBuffer
import de.huberlin.informatik.promi.util.Splitter.AsInFiles
import de.huberlin.informatik.promi.map.OneClass
import de.huberlin.informatik.promi.map.OneClassUnsupervised

sealed trait OptimizationTarget
case object F1 extends OptimizationTarget
case object AUC extends OptimizationTarget
case object Precision extends OptimizationTarget
case object WeightedF1 extends OptimizationTarget

/**
 * Factory object for GridSearch instances over one or more parameter ranges
 */
object GridSearch extends LazyLogging {

  def apply(
    params1: (String, Seq[Double]),
    f: (Double, Configuration) => Configuration): GridSearch1 = new GridSearch1(params1, f)

  def apply(
    params1: (String, Seq[Double]),
    params2: (String, Seq[Double]),
    f: (Double, Double, Configuration) => Configuration): GridSearch2 = new GridSearch2(params1, params2, f)
}

/**
 * Implementation of a grid search over various hyperparameter combinations<p>
 * By default, returns the best configuration measured by <b>AUC</b>
 *
 */
abstract class GridSearch extends LazyLogging {

  var optimizeFor: Result => Double = result => result.areaUnderROC

  def optimizeFor(target: OptimizationTarget): GridSearch = {
    optimizeFor = results => {
      target match {
        case F1 => results.fScore
        case AUC => results.areaUnderROC
        case Precision => results.precision
        case WeightedF1 => results.weightedFScore
      }
    }
    this
  }

  def run(trainingLog: XLog, baseConfig: Configuration): GridSearchResult

  def runExperiment(params: Map[String, Double], trainingLog: XLog, config: Configuration): (Result, Evaluation) = {
    logger.trace("Trying params: " + params)
    val (trainingSet, schema, filter) = WekaConverter.getInstances(trainingLog, config, "Training")
    val cvEval = new Evaluation(trainingSet)
    val collector = new PredictionCollector(trainingSet, config)
    val cvResult: Option[(Result, Evaluation)] = config.kFoldCV.map(k => {
      config.output match {
        case OneClass => {
          config.split match {
            case _: AsInFiles => crossValidatePerYear(trainingSet, config, cvEval)
            case _ => {
              cvEval.crossValidateModel(
                config.classifier.get.wekaClassifier(config, trainingSet),
                trainingSet, k,
                new java.util.Random(), collector)
              (collector.collect("CV"), cvEval)
            }
          }
        }
        case OneClassUnsupervised => {
          config.split match {
            case _: AsInFiles => testPerYear(trainingSet, config, cvEval)
            case _ => {
              val classy = config.classifier.get.wekaClassifier(config, trainingSet)
              cvEval.crossValidateModel(
                config.classifier.get.wekaClassifier(config, trainingSet),
                trainingSet, k,
                new java.util.Random(), collector)
              (collector.collect("CV"), cvEval)
            }
          }
        }
        case _ => {
          cvEval.crossValidateModel(
            config.classifier.get.wekaClassifier(config, trainingSet),
            trainingSet, k,
            new java.util.Random(), collector)
          (Result.fromEvaluation(cvEval, config.classIndex).get, cvEval)
        }
      }
    })
    cvResult.get
  }

  def comparisonValue(result: Result): Double = optimizeFor(result)

  def testPerYear(trainingSet: Instances, config: Configuration, cvEval: Evaluation) = {
    val map = mutable.Map[Int, mutable.ListBuffer[Int]]()
    val yearAttr = trainingSet.attribute("year")
    for (i <- 0 until trainingSet.numInstances()) {
      val year = trainingSet.get(i).value(yearAttr).toInt
      if (!map.contains(year)) {
        map.put(year, ListBuffer())
      }
      map.get(year).get.+=(i)
    }

    val collector = new PredictionCollector(trainingSet, config)

    for (year <- map.keys.toList.sorted) {
      val trainingFold = new Instances(trainingSet, map.get(year).get.size)
      val testFold = new Instances(trainingSet, (for (y <- map.keySet - year) yield map.get(year).size).sum)
      for (i <- map.get(year).get) {
        testFold.add(trainingSet.get(i))
      }
      val classy = config.classifier.get.wekaClassifier(config, testFold)
      classy.buildClassifier(testFold)
      cvEval.evaluateModel(classy, testFold, collector)
    }
    (collector.collect("CV"), cvEval)
  }

  def crossValidatePerYear(trainingSet: Instances, config: Configuration, cvEval: Evaluation) = {
    val map = mutable.Map[Int, mutable.ListBuffer[Int]]()
    val yearAttr = trainingSet.attribute("year")
    for (i <- 0 until trainingSet.numInstances()) {
      val year = trainingSet.get(i).value(yearAttr).toInt
      if (!map.contains(year)) {
        map.put(year, ListBuffer())
      }
      map.get(year).get.+=(i)
    }

    val collector = new PredictionCollector(trainingSet, config)

    for (year <- map.keys.toList.sorted) {
      val trainingFold = new Instances(trainingSet, map.get(year).get.size)
      val testFold = new Instances(trainingSet, (for (y <- map.keySet - year) yield map.get(year).size).sum)
      for (i <- map.get(year).get) {
        testFold.add(trainingSet.get(i))
      }
      for (y <- map.keySet - year) {
        for (i <- map.get(y).get) {
          trainingFold.add(trainingSet.get(i))
        }
      }
      val classy = config.classifier.get.wekaClassifier(config, trainingFold)
      classy.buildClassifier(trainingFold)
      cvEval.evaluateModel(classy, testFold, collector)
    }
    (collector.collect("CV"), cvEval)
  }

}

class GridSearch1(
    params1: (String, Seq[Double]),
    f: (Double, Configuration) => Configuration) extends GridSearch {

  def run(trainingLog: XLog, baseConfig: Configuration): GridSearchResult = {
    val (paramName, vals) = params1
    val results = for (i <- vals; config = f(i, baseConfig))
      yield (i, runExperiment(Map(paramName -> i), trainingLog, config), config)
    val (p1, (bestRes, bestEval), config) = results.maxBy {
      case (p1, (result, eval), config) => comparisonValue(result)
    }
    GridSearchResult(
      config, Map(paramName -> p1), bestRes, bestEval)
  }
}

class GridSearch2(
    params1: (String, Seq[Double]),
    params2: (String, Seq[Double]),
    f: (Double, Double, Configuration) => Configuration) extends GridSearch {

  def run(trainingLog: XLog, baseConfig: Configuration): GridSearchResult = {
    val (paramName1, vals1) = params1
    val (paramName2, vals2) = params2
    val results = for (i <- vals1; j <- vals2; config = f(i, j, baseConfig))
      yield (i, j, runExperiment(Map(paramName1 -> i, paramName2 -> j), trainingLog, config), config)
    val (p1, p2, (bestRes, bestEval), config) = results.maxBy {
      case (p1, p2, (result, eval), config) => comparisonValue(result)
    }
    GridSearchResult(
      config, Map(paramName1 -> p1, paramName2 -> p2), bestRes, bestEval)
  }
}

case class GridSearchResult(
  config: Configuration,
  bestParams: Map[String, Double],
  bestResult: Result,
  bestEval: Evaluation)