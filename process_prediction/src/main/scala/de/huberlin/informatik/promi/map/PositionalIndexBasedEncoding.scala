package de.huberlin.informatik.promi.map

import org.deckfour.xes.model.XTrace
import scala.collection.JavaConversions._
import weka.core._

trait PositionalIndexBasedEncoding  extends EventFeatures{
  
  override def collect(trace: XTrace): Unit = {
    values ++= trace.map { eventName(_) }
  }

  override def getAttributes(): Seq[Attribute] = {
    for (v <- values.toSeq.sorted; p <- Seq("fst", "snd", "last")) yield new Attribute(p + "_" + v)
  }

  def addAttribute(trace: XTrace, inst: Instance, data: Instances) {
    val map = scala.collection.mutable.Map[String, Seq[Int]]().withDefaultValue(Seq())
    for (i <- 0 until trace.length) {
      val event = eventName(trace(i))
      map.put(event, map(event) :+ i)     
    }
    for (v <- values) {
      val ix = map(v)
      inst.setValue(data.attribute("fst_" + v), if (ix.isDefinedAt(0)) ix(0) else -1)
      inst.setValue(data.attribute("snd_" + v), if (ix.isDefinedAt(1)) ix(1) else -1)
      inst.setValue(data.attribute("last_" + v), if (!ix.isEmpty) ix.last else -1) 
    }
  }
}