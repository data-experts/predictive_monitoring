package de.huberlin.informatik.promi.map

import org.deckfour.xes.model.XTrace
import de.huberlin.informatik.promi.xes.XESImplicits._
import scala.collection.JavaConversions._
import com.typesafe.scalalogging.LazyLogging
import java.text.SimpleDateFormat
import java.time.Year

object Output {

  def boolean(f: (XTrace => Boolean)) = new Output {
    def getCategory(trace: XTrace): String = {
      fromBoolean(f(trace))
    }
  }
}

case object OneClass extends Output {
  
  override def isOneClass = true
  
  def getCategory(trace : XTrace) = "false"
  
  override def getCategories = Seq("false")
  
}

case object OneClassUnsupervised extends Output {
  
  override def isOneClass = true
  
  def getCategory(trace : XTrace) = "false"
  
  override def getCategories = Seq("false")
  
}

trait Output extends LazyLogging {

  def isOneClass = false
  
  val T = "true"

  val F = "false"

  def fromBoolean(v: Boolean) = if (v) T else F

  def getCategories: Seq[String] = Seq(F, T)

  def getCategory(trace: XTrace): String

}

trait OutputMappings {

  val trace: XTrace

  def eventNames = trace.map { _.getSimpleEventName }

}