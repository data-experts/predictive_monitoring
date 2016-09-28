package de.huberlin.informatik.promi.experiments

import java.time.temporal.WeekFields

import de.huberlin.informatik.promi.Configuration
import de.huberlin.informatik.promi.RotationForest
import de.huberlin.informatik.promi.features.tree.ProcessTreeParams
import de.huberlin.informatik.promi.features.wekamap._
import de.huberlin.informatik.promi.map.EventBagMapping
import de.huberlin.informatik.promi.map.GenericCaseFeatures
import de.huberlin.informatik.promi.map.InstanceFilters
import de.huberlin.informatik.promi.map.Output
import de.huberlin.informatik.promi.map.Prefix
import de.huberlin.informatik.promi.map.SimpleEventNames
import de.huberlin.informatik.promi.map.SingleEvents
import de.huberlin.informatik.promi.util.Splitter
import weka.filters.unsupervised.attribute.Standardize
import de.huberlin.informatik.promi.RandomForest
import de.huberlin.informatik.promi.GridSearch
import de.huberlin.informatik.promi.Classifier
import de.huberlin.informatik.promi.Logistic
import weka.classifiers.functions.LibLINEAR
import de.huberlin.informatik.promi.LibLinear
import de.huberlin.informatik.promi.F1
import de.huberlin.informatik.promi.J48
import de.huberlin.informatik.promi.features.wekamap.ProcessTreeFeatures
import de.huberlin.informatik.promi.Weka
import de.huberlin.informatik.promi.map.IndexBasedEncoding
import de.huberlin.informatik.promi.util.CachedLogParser
import org.deckfour.xes.model.XEvent
import de.huberlin.informatik.promi.RandomForest

object BinaryClassification {

  val ridgeValues = Seq(0, 1e-8, 1e-4, 1e-2, 0.1, 0.5, 1, 10)
  val cValues = Seq(1e-8, 1e-4, 1e-2, 0.1, 0.5, 1, 10)
  val confValues = Seq(0.01, 0.05, 0.1, 0.2, 0.25, 0.3)
  val noiseValues = 0.0 to 1.0 by 0.2

  def getProcessTreeFeatures(
    noise: Float,
    profile: Seq[Profile] = 
      Count :: MinDate :: MaxDate :: DiffDate :: MinDuration :: MaxDuration :: Nil) =
    ProcessTreeFeatures(
      dateTimescale = _.get(WeekFields.ISO.weekOfYear()),
      durationTimescale = _.toMinutes(),
      imputationValue = Some(Integer.MAX_VALUE),
      profile = profile,
      params = ProcessTreeParams(noise))

  def getSimpleProcessFeatures(
    bigrams: Boolean,
    profile: Seq[Profile] = 
      Count :: MinDate :: MaxDate :: DiffDate :: MinDuration :: MaxDuration :: Nil) =
    SimpleProcessFeatures(
      dateTimescale = _.get(WeekFields.ISO.weekOfYear()),
      durationTimescale = _.toMinutes(),
      imputationValue = Some(Integer.MAX_VALUE),
      profile = profile)

  trait ParameterlessBinaryClassification extends BinaryClassification {

    def baseGridSearch: Option[GridSearch] = None

    def processGridSearch: Option[GridSearch] = Some(GridSearch(
      "n" -> noiseValues,
      (n, config) => config.copy(
        features = Seq(
          GenericCaseFeatures,
          getProcessTreeFeatures(n.toFloat)))))

  }

  trait WithRandomForestClassifier extends ParameterlessBinaryClassification {
    val classyName = "RF"
    def baseClassifier: Option[Classifier] = Some(RandomForest())
  }
  
  abstract class WithJRipper extends ParameterlessBinaryClassification {
    val classyName = "JRip"
    def baseClassifier: Option[Classifier] = Some(Weka(() => new weka.classifiers.rules.JRip()))
}

  abstract class WithDecisionTreeClassifier extends BinaryClassification {
    val classyName = "C4.5"
    def baseClassifier: Option[Classifier] = None

    def baseGridSearch: Option[GridSearch] = Some(GridSearch(
      "conf" -> confValues,
      (conf, config) => config.copy(
        classifier = Some(J48(conf.toFloat)))))

    def processGridSearch: Option[GridSearch] = Some(GridSearch(
      "n" -> noiseValues,
      "conf" -> confValues,
      (n, conf, config) => config.copy(
        classifier = Some(J48(conf.toFloat)),
        features = Seq(
          GenericCaseFeatures,
          getProcessTreeFeatures(n.toFloat)))).optimizeFor(F1))
  }

  abstract class WithL1LinearSVM extends BinaryClassification {

    val classyName = "L1-SVM"

    def baseClassifier: Option[Classifier] = None

    def baseGridSearch: Option[GridSearch] = Some(GridSearch(
      "c" -> cValues,
      (c, config) => config.copy(
        classifier = Some(LibLinear(c = c, svmType = 3)))))

    def processGridSearch: Option[GridSearch] = Some(GridSearch(
      "n" -> noiseValues,
      "c" -> cValues,
      (n, c, config) => config.copy(
        classifier = Some(LibLinear(c = c, svmType = 3)),
        features = Seq(
          GenericCaseFeatures,
          getProcessTreeFeatures(n.toFloat)))).optimizeFor(F1))

  }

  abstract class WithLogisticRegression extends BinaryClassification {

    val classyName = "Log. Regression"

    def baseClassifier: Option[Classifier] = None

    def baseGridSearch: Option[GridSearch] = Some(GridSearch(
      "r" -> ridgeValues,
      (r, config) => config.copy(
        classifier = Some(Logistic(r)))))

    def processGridSearch: Option[GridSearch] = Some(GridSearch(
      "n" -> noiseValues,
      "r" -> ridgeValues,
      (n, r, config) => config.copy(
        classifier = Some(Logistic(r)),
        features = Seq(
          GenericCaseFeatures,
          getProcessTreeFeatures(n.toFloat)))))

  }
}

trait BinaryClassification
    extends EvaluationSetting {

  def classyName: String

  def prefix: Prefix
  def output: Output
  def attributes: Seq[String]
  def plot: Plot = ROCCurve
  def instanceFilter: InstanceFilters.Filter = InstanceFilters.all

  def baseClassifier: Option[Classifier]
  def baseGridSearch: Option[GridSearch]
  def processGridSearch: Option[GridSearch]
  
  def getConfigs(): Seq[(String, Seq[(String, Configuration)])] = {
    val c0 = Configuration(
      useTestSet = true,
      filters = Seq(() => new Standardize),
      instanceFilter = instanceFilter,
      fileNames = fileNames,
      split = split,
      prefix = prefix,
      output = output,
      ignoreAtts = attributes,
      logParser = new CachedLogParser(),
      kFoldCV = Some(5),
      classifier = baseClassifier,
      gridSearch = baseGridSearch,
      features = Seq())

    val c2 = "case only" -> c0.copy(
      features = Seq(
        GenericCaseFeatures))
    val c3 = "index based 15" -> c0.copy(
      features = Seq(
        GenericCaseFeatures,
        new IndexBasedEncoding(15) with SimpleEventNames with SingleEvents))
    val c4 = "index based 30" -> c0.copy(
      features = Seq(
        GenericCaseFeatures,
        new IndexBasedEncoding(30) with SimpleEventNames with SingleEvents))
    val c5 = "bag-of-activities" -> c0.copy(
      features = Seq(
        GenericCaseFeatures,
        BinaryClassification.getSimpleProcessFeatures(false, Count :: Nil)))
    val c6 = "bag-of-activities+time" -> c0.copy(
      features = Seq(
        GenericCaseFeatures,
        BinaryClassification.getSimpleProcessFeatures(false)))
    val c7 = "tree-based " -> {
      if (!noiseTreeMiner.isDefined)
        c0.copy( gridSearch = processGridSearch)
      else 
        c0.copy(
           features = Seq(
            GenericCaseFeatures,
            BinaryClassification.getProcessTreeFeatures(noiseTreeMiner.get))    
        )
    }
    Seq(("all" -> Seq(c2, c3, c4, c5, c6, c7).map {
        case (name, config) => (classyName + " " + name) ->
          config.copy(instanceFilter = InstanceFilters.and(config.instanceFilter, InstanceFilters.all))
    }))
  }
}