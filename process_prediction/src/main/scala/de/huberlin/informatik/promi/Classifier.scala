package de.huberlin.informatik.promi

import weka.classifiers.{ Classifier => WekaClassifier }
import libsvm.svm_print_interface
import com.typesafe.scalalogging.LazyLogging
import weka.core.Instances
import weka.classifiers.meta.FilteredClassifier
import weka.filters.unsupervised.attribute.Remove
import weka.classifiers.meta.ThresholdSelector
import weka.classifiers.functions.MultilayerPerceptron
import weka.classifiers.functions.LibLINEAR
import weka.classifiers.meta.AttributeSelectedClassifier
import weka.attributeSelection._
import weka.classifiers.SingleClassifierEnhancer
import weka.clusterers.SingleClustererEnhancer
import de.huberlin.informatik.promi.map.OneClass
import weka.filters.unsupervised.instance.RemoveWithValues
import weka.filters.unsupervised.attribute.MathExpression

/**
 * Wrapper for Weka-Classifiers for easy referencing in Configuration
 */
sealed trait Classifier {

  def wekaClassifier(config: Configuration, schema: Instances): WekaClassifier = {
    val ignoreIdx = for (
      i <- 0 until schema.numAttributes();
      if config.ignoreAtts.exists(schema.attribute(i).name().matches(_))
    ) yield i

    // Remove attributes that are to be ignored during training
    val filteredClassy = new FilteredClassifier
    val filter = new Remove
    filter.setAttributeIndicesArray(ignoreIdx.toArray)
    filteredClassy.setFilter(filter)

    this match {
      case wrapper: ClassifierWrapper => 
        val enhance = wrapper.buildClassifier(config, schema)
        filteredClassy.setClassifier(wrapper.wrapped.buildClassifier(config, schema))
        enhance.setClassifier(filteredClassy)
        enhance
      case _ => 
        filteredClassy.setClassifier(buildClassifier(config, schema))
        filteredClassy
    }
  }

  def buildClassifier(config: Configuration, schema: Instances): WekaClassifier

}

abstract class ClassifierWrapper extends Classifier {
  val wrapped: Classifier
  override def buildClassifier(config: Configuration, schema: Instances): SingleClassifierEnhancer
}

case object Weka {
  def apply(classy: () => WekaClassifier) = new Classifier() {
    def buildClassifier(config: Configuration, schema: Instances) = classy()
  }
}

case class Threshold(confidenceThreshold: Double, val wrapped: Classifier) extends ClassifierWrapper {

  def buildClassifier(config: Configuration, schema: Instances) = {
    val selectorClassy = new ThresholdSelector
    selectorClassy.setOptions(Array("-C", "2")) //use second class as positive
    selectorClassy.setManualThresholdValue(confidenceThreshold)
    selectorClassy
  }
}

case class MathTransform(wrapped: Classifier, expr : String) extends ClassifierWrapper {
  
   def buildClassifier(config: Configuration, schema: Instances) = {
    val filteredClassy = new FilteredClassifier
    val filter = new MathExpression
    filter.setExpression(expr)
    filteredClassy.setFilter(filter)
    filteredClassy
  }
  
}

case class PositiveIgnorer(wrapped: Classifier) extends ClassifierWrapper {
  
  def buildClassifier(config: Configuration, schema: Instances) = {
    val filteredClassy = new FilteredClassifier
    val filter = new RemoveWithValues()
    val attLabel = schema.attribute("Label")
    filter.setAttributeIndex((attLabel.index() + 1).toString())
    filter.setNominalIndicesArr(Array(attLabel.indexOfValue("true")))
    filter.setDontFilterAfterFirstBatch(true)
    filteredClassy.setFilter(filter)
    filteredClassy
  }
  
}

case class FeatureSelection(
    wrapped: Classifier,
    numFolds: Int = 3) extends Classifier {

  def buildClassifier(config: Configuration, schema: Instances) = {
    val attrClassy = new AttributeSelectedClassifier()
    val classy = wrapped.buildClassifier(config, schema)
    val eval = new WrapperSubsetEval
    eval.setOptions(Array("-E", "auc"))
    eval.setFolds(numFolds)
    eval.setClassifier(classy)
    val search = new BestFirst
    search.setStartSet("1")
    attrClassy.setSearch(search)
    attrClassy.setEvaluator(eval)
    attrClassy.setClassifier(classy)
    attrClassy
  }
}

case object NaiveBayes extends Classifier {
  def buildClassifier(config: Configuration, schema: Instances) = new weka.classifiers.bayes.NaiveBayes()
}

case class J48(conf: Float = -1) extends Classifier {
  def buildClassifier(config: Configuration, schema: Instances) = {
    val tree = new weka.classifiers.trees.J48
    if (conf != -1) tree.setConfidenceFactor(conf)
    tree.setOptions(Array("-A"))
    tree
  }
}

case class LibSVM(
    options: List[String] = Nil,
    c: Double = -1,
    normalize: Boolean = true,
    gamma: Double = -1) extends Classifier with LazyLogging {

  libsvm.svm.svm_set_print_string_function(new svm_print_interface() {
    def print(s: String) = ()//logger.debug(s)
  })

  def buildClassifier(config: Configuration, schema: Instances) = {
    val svm = new weka.classifiers.functions.LibSVM
    svm.setOptions(options.toArray)
    if (c != -1) svm.setCost(c)
    if (gamma != -1) svm.setGamma(gamma)
    svm
  }
}

/**
 * svmType:<br>
 * <ul>
 * <li>0 = L2-regularized logistic regression
 * <li>1 = L2-loss support vector machines (dual)
 * <li>2 = L2-loss support vector machines (primal)
 * <li>3 = L1-loss support vector machines (dual)
 * <li>4 = multi-class support vector machines by Crammer and Singer
 * </ul>
 */
case class LibLinear(
    c: Double = -1,
    svmType: Int = 2) extends Classifier {

  def buildClassifier(config: Configuration, schema: Instances) = {
    val l = new LibLINEAR()
    l.setOptions(Array("-S", svmType.toString(), "-W", "1"))
    if (c != -1) l.setCost(c)
    l
  }
}

case class Logistic(ridge: Double) extends Classifier {
  def buildClassifier(config: Configuration, schema: Instances) = {
    val l = new weka.classifiers.functions.Logistic
    l.setRidge(ridge)
    l
  }
}

case object BayesNet extends Classifier {
  def buildClassifier(config: Configuration, schema: Instances) = {
    val bn = new weka.classifiers.bayes.BayesNet
    bn
  }
}

case class NeuralNetwork(hiddenLayers: Seq[Int]) extends Classifier {

  def buildClassifier(config: Configuration, schema: Instances) = {
    val nn = new MultilayerPerceptron
    nn.setHiddenLayers(hiddenLayers.mkString(","))
    nn
  }

}

case object RotationForest extends Classifier {
  def buildClassifier(config: Configuration, schema: Instances) = {
    val rf = new weka.classifiers.meta.RotationForest
    rf
  }
}

case class RandomForest(numFeatures: Int = -1, maxDepth: Int = -1, numIterations: Int = 500) extends Classifier {
  def buildClassifier(config: Configuration, schema: Instances) = {
    val rf = new weka.classifiers.trees.RandomForest
    //    rf.setOptions(Array("-K", "1"))
    if (numFeatures != -1) rf.setNumFeatures(numFeatures)
    if (maxDepth != -1) rf.setMaxDepth(maxDepth)
    if (numIterations != -1) rf.setNumIterations(numIterations)
    rf.setNumExecutionSlots(Runtime.getRuntime.availableProcessors())
    rf
  }
}