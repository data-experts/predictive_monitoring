package de.huberlin.informatik.promi.map

import weka.core.Instance
import weka.core.Instances
import scala.collection.mutable

import de.huberlin.informatik.promi.xes.XESImplicits._
import scala.collection.JavaConversions._
import org.deckfour.xes.model._
import com.typesafe.scalalogging.LazyLogging
import weka.core.Attribute
import java.text.SimpleDateFormat
import java.util.concurrent.atomic.AtomicInteger
import weka.gui.SimpleDateFormatEditor
import java.time.ZoneOffset
import de.huberlin.informatik.promi.features.wekamap.CaseFeatures

case object CountPerDepartment extends CaseFeatures {

  val deps = mutable.Map[(String, Int), Int]().withDefaultValue(0)

  override def clear = deps.clear()

  override def collect(trace: XTrace): Unit = {
    val dep = trace.getString("dep").getOrElse("?")
    val year = trace.getInteger("year").getOrElse(0)
    deps.put((dep, year), deps((dep, year)) + 1)
  }

  def getAttributes(): Seq[Attribute] =
    return Seq(new Attribute("count_dep"))

  def addAttribute(trace: XTrace, inst: Instance, data: Instances): Unit = {
    val dep = trace.getString("dep").getOrElse("?")
    val year = trace.getInteger("year").getOrElse(0)
    inst.setValue(data.attribute("count_dep"), deps((dep, year)))
  }

}

case object NumberUnsuccessful extends CaseFeatures {

  val key = "numUnsuccesful"

  override def clear = {}

  def addAttribute(trace: XTrace, inst: Instance, data: Instances) = {
    val count = trace.count { _.getAttributes.get("successful").toString.toBoolean.unary_! }
    inst.setValue(data.attribute(key), count)
  }

  def getAttributes(): Seq[Attribute] = Seq(new Attribute(key))

}

object GenericCaseFeatures extends CaseFeatures with LazyLogging {

  val numericFeatures = mutable.TreeSet[String]()

  val booleanFeatures = mutable.TreeSet[String]()

  val discreteFeatures = mutable.Map[String, Set[String]]().withDefaultValue(Set())

  override def clear = {
    numericFeatures.clear()
    booleanFeatures.clear()
    discreteFeatures.clear()
  }

  override def collect(trace: XTrace): Unit = {
    for (entry <- trace.getAttributes) {
      entry._2 match {
        case b: XAttributeBoolean => booleanFeatures += b.getKey
        case n: XAttributeContinuous => numericFeatures += n.getKey
        case n: XAttributeDiscrete => numericFeatures += n.getKey
        case l: XAttributeLiteral if l.getKey != "concept:name" => discreteFeatures.update(
          l.getKey, discreteFeatures(l.getKey) + l.getValue)
        case _: XAttributeLiteral => //ignore concept:name
        case any => logger.warn("Unknown element: " + any.getClass)
      }
    }
  }

  def addAttribute(trace: XTrace, inst: Instance, data: Instances) = {
    for (entry <- trace.getAttributes) {
      entry._2 match {
        case b: XAttributeBoolean => inst.setValue(data.attribute(b.getKey), if (b.getValue) 1 else 0)
        case n: XAttributeContinuous if data.attribute(n.getKey) != null =>
          inst.setValue(data.attribute(n.getKey), n.getValue)
        case n: XAttributeDiscrete => inst.setValue(data.attribute(n.getKey), n.getValue)
        case l: XAttributeLiteral if l.getKey != "concept:name" &&
          (data.attribute(l.getKey).indexOfValue(l.getValue) != -1) =>
          inst.setValue(data.attribute(l.getKey), l.getValue)
        case ignore =>
      }
    }
  }

  def getAttributes(): Seq[Attribute] =
    Seq(
      discreteFeatures.map(values => {
        val key = values._1
        val vector = new java.util.ArrayList[String]()
        if (key == "year") vector.addAll(2007 to 2013 map (_.toString))
        else values._2.foreach {
          vector.add(_)
        }
        new Attribute(key, vector)
      }),
      numericFeatures.map { new Attribute(_) },
      booleanFeatures.map { new Attribute(_) }).flatten

}

trait EventFeatures extends CaseFeatures {

  def getEventNGrams(trace: XTrace): Map[String, Int] = ???

  def getAttributes = values.toSeq.sorted.map(newAttribute(_)).toSeq

  def eventName(event: XEvent): String

  def prefix: String = ""

  def newAttribute(name: String) = new Attribute(name)

}

object TraceLength extends CaseFeatures {

  override def collect(trace: XTrace): Unit = {}

  override def getAttributes() = Seq(new Attribute("trace_length"))

  def addAttribute(trace: XTrace, inst: Instance, data: Instances) = {
    val prefixLength = trace.length
    inst.setValue(data.attribute("trace_length"), prefixLength)
  }
}

object FP200Connectedness extends CaseFeatures {

  override def collect(trace: XTrace): Unit = {
    values ++= trace.map(_.getSimpleEventName).filter(!_.contains("FP200"))
  }

  override def getAttributes() = values.toSeq.sorted.map(v => new Attribute("cnnt_" + v))

  def addAttribute(trace: XTrace, inst: Instance, data: Instances) = {
    val fp200Beginning = trace.map(_.getSimpleEventName()).dropWhile { _ != "FP200 DokumentBearbeitungBeenden" }
    for (v <- values) {
      inst.setValue(data.attribute("cnnt_" + v), fp200Beginning.count(_ == v))
    }
  }

}

object AvgPerDay extends CaseFeatures {

  override def getAttributes() = Seq(new Attribute("avg_day"))

  val sdf = new SimpleDateFormat("DDD")

  def addAttribute(trace: XTrace, inst: Instance, data: Instances) = {
    val days = trace.map(e => sdf.format(e.getTimestamp())).groupBy(identity).map {
      case (day, num) => (day, num.size)
    }.toMap
    inst.setValue(data.attribute("avg_day"), days.values.sum / days.keys.size)
  }

}

object CuttingPerArea extends CaseFeatures {

  override def getAttributes() = Seq(new Attribute("ctper_area"))

  def addAttribute(trace: XTrace, inst: Instance, data: Instances) = {
    val value = (for (
      c <- trace.getDouble("cutting0");
      a <- trace.getDouble("area");
      if a != 0d
    ) yield (c / a)).getOrElse(0d)
    inst.setValue(data.attribute("ctper_area"), value)
  }

}

object AverageParcelSize extends CaseFeatures {

  override def getAttributes() = Seq(new Attribute("avg_parcelsize"))

  def addAttribute(trace: XTrace, inst: Instance, data: Instances) = {
    val value = (for (
      a <- trace.getDouble("area");
      n <- trace.getDouble("numparcels");
      if n != 0d
    ) yield (a / n)).getOrElse(0d)
    inst.setValue(data.attribute("avg_parcelsize"), value)
  }
}

object GueltigSetzer extends CaseFeatures {

  override def collect(trace: XTrace): Unit = {
    values += getGueltigSetzer(trace)
  }
  
  def getGueltigSetzer(trace : XTrace) : String = 
    trace.find { _.getSimpleEventName().contains("DokumentGueltig") }.
      map(_.getAttributes.get("org:resource").toString).getOrElse("Unknown")
  
  override def getAttributes() = Seq(new Attribute("gueltigres", values.toList))

  def addAttribute(trace: XTrace, inst: Instance, data: Instances) = {
     val att = data.attribute("gueltigres")
     val user = getGueltigSetzer(trace)
     if (att.indexOfValue(user) != -1) inst.setValue(att, user)
  }

}

object ActionsPerParcel extends CaseFeatures {

  override def getAttributes() = Seq(new Attribute("actions_per_parcel"))

  def addAttribute(trace: XTrace, inst: Instance, data: Instances) = {
    val actions = trace.filter { _.getSimpleEventName().contains("FM") }.size
    val value = trace.getDouble("numparcels").filter(_ != 0.0).map { p => actions / p }.getOrElse(0d)
    inst.setValue(data.attribute("actions_per_parcel"), value)
  }

}

case class EventCount(name: String) extends CaseFeatures {

  override def collect(trace: XTrace): Unit = {
    values ++= trace.map(_.getSimpleEventName()).filter { _.contains(name) }   
  }
  
  override def getAttributes() = values.toSeq.sorted.map(v => new Attribute(v))

  def addAttribute(trace: XTrace, inst: Instance, data: Instances) = {
    for (v <- values) {
      inst.setValue(data.attribute(v), trace.count(_.getSimpleEventName().contains(v)))
    }
  }

}

object Antragseingang extends CaseFeatures {

  def getAttributes() = Seq(new Attribute("entry"))

  def addAttribute(trace: XTrace, inst: Instance, data: Instances) = {
    val peb = trace.find { _.getSimpleEventName().contains("DokumentEingegangen") }.map(_.getTimestamp)
    if (!peb.isDefined) {
      inst.setValue(data.attribute("entry"), 999)
    } else {
      val year = trace.getInteger("year").get
      val sdf = new SimpleDateFormat("D")
      val ddd = sdf.format(peb.get).toInt
      val terminDate = new SimpleDateFormat("dd.MM.yyyy").parse("15.05." + year)
      val termin = sdf.format(terminDate).toInt

      val value = if ((termin - ddd) > 0) 0.0 else 1.0
      inst.setValue(data.attribute("entry"), value)
    }
  }
}

object BerechnungInterruptions extends CaseFeatures {

  override def collect(trace: XTrace): Unit = {
    //values.addAll(trace.map(_.getEventName()).filter(events.contains(_)))   
  }

  override def getAttributes() = Seq(new Attribute("berechnung_episode"))

  def addAttribute(trace: XTrace, inst: Instance, data: Instances) = {
    var i = -1
    var maxLength = 0
    var currLength = 0
    for (j <- 0 until trace.length) {
      if (trace(j).getSimpleEventName.contains("FM")) currLength += 1
      if (trace(j).getSimpleEventName == "FP200 BerechnenUndPruefen") {
        if (i != -1) {
          maxLength = maxLength.max(currLength)
        }
        currLength = 0
        i = j
      }
    }
    inst.setValue(data.attribute("berechnung_episode"), maxLength)
  }
}

case class DocTypeFeatures(considered: Seq[String] = Seq("")) extends CaseFeatures {

  def getDocTypes(trace: XTrace): Seq[String] =
    trace.map(_.getDocType()).filter(d => considered.exists { s => d.contains(s) })

  override def collect(trace: XTrace): Unit = {
    val doctypes = getDocTypes(trace)
    values.addAll(doctypes)
  }

  override def getAttributes() =
    values.toList.map(s => new Attribute("count_" + s)) ++ 
    Seq(new Attribute("numDocSwitches"))

  def addAttribute(trace: XTrace, inst: Instance, data: Instances) = {
    def toDouble(b: Boolean) = if (b) 1.0 else 0.0

    var numDocs = 0
    var doc: String = null
    val count = mutable.Map[String, Int]().withDefaultValue(0)
    for (curDoc <- getDocTypes(trace)) {
      if (doc != curDoc) {
        numDocs += 1
      }
      doc = curDoc
      count(curDoc) = count(curDoc) + 1
    }
    inst.setValue(data.attribute("numDocSwitches"), numDocs)
    for (s <- values) {
      val att = data.attribute("count_" + s)
      if (att != null) inst.setValue(att, count(s))
    }
  }
}

trait SingleEvents extends EventFeatures {

  override def getEventNGrams(trace: XTrace) =
    trace.groupBy(eventName(_)).mapValues { _.size }

  override def collect(trace: XTrace): Unit = {
    values ++= trace.map(eventName(_))
  }
}

trait EventBiGrams extends EventFeatures {

  def getBiGrams(trace: XTrace): Seq[String] = {
//    val prefix = trace.filter(e => trace.count(_.getEventName() == e.getEventName()) > 4)
    val prefix = trace
    if (prefix.isEmpty) Seq()
    else prefix.zip(prefix.tail).map(p => eventName(p._1) + eventName(p._2))
  }

  override def getEventNGrams(trace: XTrace) = {
    getBiGrams(trace).groupBy(identity).mapValues { _.size }
  }

  override def collect(trace: XTrace): Unit = {
    values ++= getBiGrams(trace)
  }
}

trait EventTriGrams extends EventFeatures {

  def getTriGrams(trace: XTrace): Seq[String] = {
    val prefix = trace
    prefix.zip(prefix.tail).zip(prefix.tail.tail).map(
      p => eventName(p._1._1) + eventName(p._1._2) + eventName(p._2))
  }

  override def getEventNGrams(trace: XTrace) = {
    getTriGrams(trace).groupBy(identity).mapValues { _.size }
  }

  override def collect(trace: XTrace): Unit = {
    values ++= getTriGrams(trace)
  }
}

trait EventBagMapping extends EventFeatures {

  def addAttribute(trace: XTrace, inst: Instance, data: Instances): Unit = {
    val count = getEventNGrams(trace)
    for (event <- values) {
      val att = data.attribute(event)
      inst.setValue(att, count.getOrElse(event, 0))
    }
  }

  override def toString = "Events as Bag"
}

trait EventSetMapping extends EventFeatures {

  def addAttribute(trace: XTrace, inst: Instance, data: Instances): Unit = {
    val count = getEventNGrams(trace).mapValues { _ => 1 }
    for (event <- values) {
      val att = data.attribute(event)
      inst.setValue(att, count.getOrElse(event, 0))
    }
  }

  override def toString = "Events as Set"
}

trait SimpleEventNames {
  this: EventFeatures =>
  def eventName(event: XEvent) = prefix + "_" + event.getSimpleEventName
}

trait CompositeEventNames {
  this: EventFeatures =>
  def eventName(event: XEvent) = prefix + "_" + event.getCompositeEventName("doctype")
}

object Execution {
  val weekOfYear = "w" // Kalenderwoche
}

trait MinMaxDuration extends EventFeatures {

  override def getEventNGrams(trace: XTrace): Map[String, Int] = ???

  override def collect(trace: XTrace): Unit = {
    values ++= trace.map(eventName(_))
  }

  def toTimestamp(e: XEvent): Long = e.getTimestamp().toEpochSecond(ZoneOffset.UTC)

  def addAttribute(trace: XTrace, inst: Instance, data: Instances): Unit = {
    trace.
      groupBy(eventName(_)).mapValues(events => {
        val max = events.map { toTimestamp(_) }.max
        val min = events.map { toTimestamp(_) }.min
        max - min
      }).foreach {
        case (name, diff) =>
          val att = data.attribute(name)
          if (att != null) {
            inst.setValue(att, diff)
          }
      }
  }

  override def prefix = "diff_"
}

abstract class SingleExecution extends EventFeatures {

  override def getEventNGrams(trace: XTrace): Map[String, Int] = ???

  override def collect(trace: XTrace): Unit = {
    values ++= trace.filter(filter(_)).map(eventName(_))
  }

  override def getAttributes =
    values.toSeq.sorted.
      flatMap(e => List("min", "max").map(_ + e)).
      map(newAttribute(_)).toSeq
      
  def filter(event: XEvent): Boolean = Seq("200", "VWK").exists { p => event.getSimpleEventName().contains(p) } 

  val sdf = new SimpleDateFormat(Execution.weekOfYear)

  def toTimestamp(e: XEvent): Long = sdf.format(e.getTimestamp()).toInt

  def addAttribute(trace: XTrace, inst: Instance, data: Instances): Unit = {
    trace.
      filter(filter(_)).
      groupBy(eventName(_)).mapValues(_.map(toTimestamp(_))).foreach {
        case (name, timestamps) =>
          setAttr("min", _.min, name, timestamps, inst, data)
          setAttr("max", _.max, name, timestamps, inst, data)
        //        setAttr("mean", StatsUtil.mean(_), name, timestamps, inst, data)
        //        setAttr("median", StatsUtil.median(_), name, timestamps, inst, data)
      }
  }

  def setAttr(prefix: String,
    aggregator: Seq[Long] => Double, name: String, timestamps: Seq[Long], inst: Instance, data: Instances) = {
    val att = data.attribute(prefix + name)
    if (att != null) {
      inst.setValue(att, aggregator(timestamps))
    }
  }
}

trait ELERVorEntscheidung {
  this: EventFeatures =>
  def stoppingCriterion(event: XEvent): Boolean =
    event.getSimpleEventName() == "FOERDERANTRAG Bewilligen" ||
      event.getSimpleEventName() == "FOERDERANTRAG FAAblehnen"
}
