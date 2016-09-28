package de.huberlin.informatik.promi.map

import weka.core.Instances
import org.deckfour.xes.model.XTrace
import weka.core.Instance
import weka.core.Attribute
import scala.collection.JavaConversions._


abstract class IndexBasedEncoding(val maxLength: Int) extends EventFeatures {

  override def getEventNGrams(trace: XTrace): Map[String, Int] = ???

  override def collect(trace: XTrace): Unit = {
    val p = trace
    values ++= p.map { eventName(_) }
  }

  override def getAttributes(): Seq[Attribute] = {
    for (i <- 1 to maxLength) yield (new Attribute("index_" + i, "unknown" :: values.toList))
  }

  def addAttribute(trace: XTrace, inst: Instance, data: Instances) {
    for ((i, event) <- 1 to maxLength zip trace) {
      val name = eventName(event)
      if (data.attribute("index_" + i).indexOfValue(name) != -1) {
        inst.setValue(data.attribute("index_" + i), eventName(event))
      } else {
        inst.setValue(data.attribute("index_" + i), "unknown")
      }
    }
  }

}