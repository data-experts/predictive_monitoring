package de.huberlin.informatik.promi.util

import scala.collection.JavaConversions.asScalaBuffer
import scala.collection.JavaConversions.seqAsJavaList

import org.deckfour.xes.model.XLog
import org.deckfour.xes.model.XTrace

import de.huberlin.informatik.promi.Configuration
import weka.core.Attribute
import weka.core.DenseInstance
import weka.core.Instances
import weka.filters.Filter
import org.deckfour.xes.factory.XFactoryRegistry
import de.huberlin.informatik.promi.features.wekamap.CaseFeatures
import weka.core.Instance

/**
 * converts XES logs to WEKA instances by mapping them into the feature space
 */
object WekaConverter {

  /**
   * Converts a log to WEKA using {@code schema} and {@code filter}<p>
   * {@code schema} is usually a set of training instances, used to learn the feature map.
   * Thus, this method is meant to be applied to validation and test sets.
   */
  def getInstances(log: XLog, config: Configuration, title: String, schema: Instances, filter: Filter): Instances = {
    val atts = new java.util.ArrayList[Attribute](schema.numAttributes())

    for (i <- 0 until schema.numAttributes()) {
      atts.add(schema.attribute(i))
    }

    val dataSet = new Instances(title, atts, log.size())
    dataSet.setClassIndex(schema.numAttributes() - 1)

    log.par.map(getInstance(_, dataSet, config)).seq.foreach { dataSet.add(_) }

    filter.batchFinished()
    Filter.useFilter(dataSet, filter)
  }

  /**
   * Converts a log to WEKA instances using the mapping given by {@code config}<p>
   *
   * Returns a triple consisting of:
   * <ol>
   * <li>the filtered training set that can be handed to a WEKA learning algorithm
   * <li>the unfiltered training set, that can be used to transform validation and test sets in the same format
   * <li>the filter, that should be applied to validation and test sets after conversion
   * </ol>
   * Note that "filtering" can also involve dimensionality reducation.<br>
   * Hence the need to return two sets of instances.
   */
  def getInstances(log: XLog, config: Configuration, title: String): (Instances, Instances, Filter) = {
    config.features.foreach(_.clear)

    // Init Attributes
    val atts = new java.util.ArrayList[Attribute]()

    getAttributes(log, config).foreach { atts.add(_) }

    val classVector = config.output.getCategories

    val classAttr = new Attribute("__class", classVector)
    atts.add(classAttr)

    // Init Training Set
    val dataSet = new Instances(title, atts, log.size())

    val numAtts = dataSet.numAttributes()
    dataSet.setClassIndex(numAtts - 1)

    log.par.map(trace => {
      val inst = getInstance(trace, dataSet, config)
      if (inst.classValue() == 1.0d) {
        inst.setWeight(inst.weight() * config.positiveMultiplier)
      }
      inst
    }).seq.foreach { dataSet.add(_) }

    val filter = config.wekaFilter;
    filter.setInputFormat(dataSet);
    (Filter.useFilter(dataSet, filter), dataSet, filter)
  }

  private def getInstance(trace: XTrace, data: Instances, config: Configuration) = {
    val numAtts = data.numAttributes()
    val inst = new DenseInstance(numAtts)

    inst.setDataset(data)
    val label = config.output.getCategory(trace)
    inst.setClassValue(label)
    if (config.features.isEmpty) throw new IllegalArgumentException("Feature map must be provided")
    LabelMapper(config).addAttribute(trace, inst, data)
    for (mapper <- config.features) {
      mapper.addAttribute(config.prefix.getPrefix(trace), inst, data)
    }

    inst
  }

  private def getAttributes(log: XLog, config: Configuration): Seq[Attribute] = {
    config.features.foreach(_.init(log))
    for (
      trace <- log;
      caseMapper <- config.features
    ) { caseMapper.collect(config.prefix.getPrefix(trace)) }
    (LabelMapper(config) +: config.features).flatMap(_.getAttributes)

  }

  case class LabelMapper(config: Configuration) extends CaseFeatures {

    def getAttributes(): Seq[Attribute] = {
      val labels = (config.label match {
        case Some(out) => out.getCategories
        case None => config.output.getCategories
      })
      Seq(new Attribute("Label", labels))
    }

    def addAttribute(trace: XTrace, inst: Instance, data: Instances) = {
      val label = (config.label match {
        case Some(out) => out.getCategory(trace)
        case None => config.output.getCategory(trace)
      })
      inst.setValue(data.attribute("Label"), label)
    }

  }

}