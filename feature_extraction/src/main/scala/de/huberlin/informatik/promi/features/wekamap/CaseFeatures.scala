package de.huberlin.informatik.promi.features.wekamap

import org.deckfour.xes.model.XTrace
import org.deckfour.xes.model.XLog
import weka.core.Instances
import weka.core.Attribute
import weka.core.Instance
import scala.collection.mutable

trait CaseFeatures {

  val values = mutable.TreeSet[String]()

  def clear = {
    values.clear()
  }

  def collect(trace: XTrace): Unit = {}

  def init(training : XLog) = {}
  
  def getAttributes(): Seq[Attribute]

  def addAttribute(trace: XTrace, inst: Instance, data: Instances): Unit

}