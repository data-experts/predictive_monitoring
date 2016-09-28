package de.huberlin.informatik.promi.experiments.outlier

import de.huberlin.informatik.promi._
import map._
import util._
import de.huberlin.informatik.promi.experiments._
import weka.filters.unsupervised.attribute.Standardize
import de.huberlin.informatik.promi.features.tree.ProcessTreeParams
import weka.classifiers.functions.LibLINEAR
import de.huberlin.informatik.promi.features.wekamap._
import java.time.temporal.WeekFields
import de.huberlin.informatik.promi.features.wekamap.ProcessFeatures
import de.huberlin.informatik.promi.map.GenericCaseFeatures
import org.deckfour.xes.model.XEvent
import java.awt.EventFilter
import weka.filters.unsupervised.attribute.MathExpression
import weka.filters.unsupervised.attribute.Normalize

trait Outlier extends EvaluationSetting {

  def gridSearch: Option[GridSearch]

  def plot: Plot = NoPlot

  val gammaVals = Seq(0.001, 0.005, 0.01, 0.1, 0.5, 1)
  
  def nu : Double

  def prefix: Prefix
  def output: Output
  def attributes: Seq[String]

  def wrapper(classy: Classifier): Classifier =
    classy
    
  def classifier : Option[Classifier] = None

  def getConfigs(): Seq[(String, Seq[(String, Configuration)])] = {
    val c0 = Configuration(
      filters = Seq(() => new Normalize),
      fileNames = fileNames,
      split = split,
      prefix = prefix,
      output = OneClassUnsupervised,
      label = Some(output),
      ignoreAtts = AttributeSets.includeAll,
      logParser = new CachedLogParser(),
      kFoldCV = Some(5),
      classifier = classifier,
      features = Seq(),
      useTestSet = true,
      gridSearch = gridSearch)

    val c1 = "w/o events" -> c0.copy(
      features = Seq(
        GenericCaseFeatures))
    val c2 = "w/ simple Events" -> c0.copy(
      features = Seq(
        GenericCaseFeatures,
        SimpleProcessFeatures(
          profile = Count :: Nil)))
    val c3 = "w/ simple events + ts" -> c0.copy(
      features = Seq(
        GenericCaseFeatures,
        SimpleProcessFeatures(
          dateTimescale = _.getDayOfYear,
          durationTimescale = _.toMinutes(),
          profile = Count :: MaxDate :: DiffDate :: Nil)))
    val c4 = "w/ timestamps w/o count" -> c0.copy(
      features = Seq(
        GenericCaseFeatures,
        SimpleProcessFeatures(
          dateTimescale = _.getDayOfYear,
          durationTimescale = _.toMinutes(),
          profile = MinDate :: MaxDate :: DiffDate :: Nil)))
    val c5 = "w/ tree based events" -> c0.copy(
      features = Seq(
        GenericCaseFeatures,
        ProcessTreeFeatures(
          dateTimescale = _.getDayOfYear,
          durationTimescale = _.toMinutes(),
          profile = Count :: MinDate :: MaxDate :: DiffDate :: MinDuration :: MaxDuration :: Nil,
          params = ProcessTreeParams(noiseTreeMiner.get))))
    val c6 = "w/ tree based events w/o count" -> c0.copy(
      features = Seq(
        GenericCaseFeatures,
        ProcessTreeFeatures(
          dateTimescale = _.getDayOfYear,
          durationTimescale = _.toMinutes(),
          profile = MinDate :: MaxDate :: DiffDate :: MinDuration :: MaxDuration :: Nil,
          params = ProcessTreeParams(noiseTreeMiner.get))))

    Seq("all" -> Seq(c1, c2, c3, c4, c5, c6).map {
      case (name, config) => (classyName + " " + name) -> config.copy(instanceFilter = InstanceFilters.all)
    })
  }
}