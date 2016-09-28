package de.huberlin.informatik.promi.features.wekamap

import java.time.ZoneOffset

import scala.collection.JavaConversions.asScalaBuffer
import scala.collection.JavaConversions.bufferAsJavaList

import org.deckfour.xes.factory.XFactory
import org.deckfour.xes.factory.XFactoryRegistry
import org.deckfour.xes.model.XEvent
import org.deckfour.xes.model.XLog
import org.deckfour.xes.model.XTrace

import de.huberlin.informatik.promi.features.tree.ProcessTreeDecomposer
import de.huberlin.informatik.promi.features.tree.ProcessTreeParams
import de.huberlin.informatik.promi.xes.XESImplicits.toRichEvent
import org.deckfour.xes.extension.std.XConceptExtension

/**
 * WEKA wrapper to collect a number of traces from an event logs, construct features out of a process tree and fill
 * the corresponding attributes on a WEKA instace
 *
 * This class wraps a mutable log. Hence, all operations but #addAttribute are not thread safe
 */
case class ProcessTreeFeatures(
    val params: ProcessTreeParams,
    val profile: Seq[Profile],
    val classifier: XEvent => String = _.getSimpleEventName(),
    val eventFilter: XEvent => Boolean = _ => true,
    val traceFilter: XTrace => Boolean = _ => true,
    val ignoreLeaves: Boolean = false,
    val normalizeDurations: Boolean = false,
    val durationTimescale: java.time.Duration => Double = _.toDays(),
    val dateTimescale: java.time.LocalDateTime => Double = _.toEpochSecond(ZoneOffset.UTC),
    val imputationValue: scala.Option[Double] = None,
    val treePrefix: String = "") extends ProcessFeatures {

  val prefix = treePrefix + "ptree"
  
  var log = XFactoryRegistry.instance().currentDefault().createLog()

  def getXFactory: XFactory = XFactoryRegistry.instance().currentDefault()

  def getFeatureMap = new ProcessTreeDecomposer(log, params).getFeatureMap()

  override def init(training: XLog) = {
    log.setAttributes(training.getAttributes)
    log.getClassifiers.addAll(training.getClassifiers)
  }

  override def clear() = {
    super.clear
    log.clear()
  }

  override def collect(trace: XTrace): Unit = {
    if (traceFilter(trace)) {
      val filtered = getXFactory.createTrace(trace.getAttributes)
      filtered.addAll(trace.filter(eventFilter).filter { _.getBoolean("successful").get }.map(event => {
        val newEvent = getXFactory.createEvent(event.getAttributes)
        newEvent.getAttributes.put(XConceptExtension.KEY_NAME, getXFactory.createAttributeLiteral(
          XConceptExtension.KEY_NAME, classifier(event), XConceptExtension.instance()))
        newEvent
      }))
      log.add(filtered)
    }
  }
}