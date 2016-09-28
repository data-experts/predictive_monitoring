package de.huberlin.informatik.promi.map

import org.deckfour.xes.model.XTrace
import org.deckfour.xes.model.XEvent
import org.deckfour.xes.factory.XFactoryRegistry

trait Prefix {

  def getPrefix(trace: XTrace): XTrace

}

case class FilterEvents(
   prefix : Prefix,
   filter : XEvent => Boolean
) extends Prefix {
  import scala.collection.JavaConversions._
  def getPrefix(trace : XTrace) = {
    val p = prefix.getPrefix(trace)
    val prefixed = XFactoryRegistry.instance().currentDefault().createTrace(trace.getAttributes)
    prefixed.addAll(p.filter(filter))
    prefixed
  }
    
}