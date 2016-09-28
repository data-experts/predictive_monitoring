package de.huberlin.informatik.promi

package xes

import org.deckfour.xes.model.XEvent
import org.deckfour.xes.extension.std.XConceptExtension
import org.deckfour.xes.model.XTrace
import scala.collection.JavaConversions._
import org.deckfour.xes.extension.std.XTimeExtension
import org.deckfour.xes.model.XAttributeTimestamp
import org.deckfour.xes.factory.XFactoryRegistry
import java.time.ZoneId

/**
 * Implicit conversions to pimp the OpenXES library with frequently used operations
 */
trait XESImplicits {

  implicit def toRichTrace(trace: XTrace) = new RichTrace(trace)

  implicit def toRichEvent(event: XEvent) = new RichEvent(event)

  implicit val dateTimeOrd = new Ordering[java.time.LocalDateTime] {
    def compare(t1 : java.time.LocalDateTime, t2 : java.time.LocalDateTime) = t1.compareTo(t2)
  }
  
  class RichEvent(val event: XEvent) {

    def getCompositeEventName(attribute: String) = event.getAttributes.get(attribute).toString + "_" + event.getAttributes.get(XConceptExtension.KEY_NAME).toString

    def getSimpleEventName() = event.getAttributes.get(XConceptExtension.KEY_NAME).toString

    def getDocType() = event.getAttributes.get("doctype").toString

    def getTimestamp(): java.time.LocalDateTime =
      event.
        getAttributes.get(XTimeExtension.KEY_TIMESTAMP).
        asInstanceOf[XAttributeTimestamp].getValue.toInstant().
        atZone(ZoneId.systemDefault()).toLocalDateTime()

    def getBoolean(key: String): Option[Boolean] = getString(key).map(_.toBoolean)
    def getString(key: String): Option[String] = event.getAttributes.toMap.get(key).map(_.toString)

  }

  class RichTrace(val trace: XTrace) {

    def getPrefix(stoppingCriterion: XEvent => Boolean): XTrace = {
      val events = trace.takeWhile(!stoppingCriterion(_))
      val prefixed = XFactoryRegistry.instance().currentDefault().createTrace(trace.getAttributes)
      prefixed.addAll(events)
      prefixed
    }

    def getInteger(key: String): Option[Int] = getString(key).map(_.toInt)
    def getDouble(key: String): Option[Double] = getString(key).map(_.toDouble)
    def getBoolean(key: String): Option[Boolean] = getString(key).map(_.toBoolean)
    def getString(key: String): Option[String] = trace.getAttributes.toMap.get(key).map(_.toString)

  }
}

object XESImplicits extends XESImplicits