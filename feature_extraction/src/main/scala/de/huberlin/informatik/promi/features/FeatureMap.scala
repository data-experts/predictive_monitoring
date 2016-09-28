package de.huberlin.informatik.promi.features

import org.deckfour.xes.model.XTrace
import java.util.Date
import org.deckfour.xes.model.XEvent
import de.huberlin.informatik.promi.xes.XESImplicits._

trait FeatureMap {
  
  def listFeatures : Seq[FeatureType]
  
  def apply(trace : XTrace) : Seq[Features]
  
}

abstract class Features(val featureType : FeatureType) {
  val matches : Seq[Seq[XEvent]]
  
  override def toString = {
    featureType.name + ": " + matches.map(_.map { _.getSimpleEventName }.mkString("(", "-", ")")).mkString("[", ",", "]")
  }
  
}

trait FeatureType {
   val name : String
   def hasDuration : Boolean
   override def toString = name
}

case class SequenceFeatures(
    name : String,
    matches : Seq[Seq[XEvent]]
) extends Features(Sequence(name))

case class LoopFeatures(
   name : String,
   matches : Seq[Seq[XEvent]]
) extends Features(Loop(name))

case class AndFeatures(
   name : String,
   matches : Seq[Seq[XEvent]]
) extends Features(And(name))

case class XorFeatures(
   name : String,
   matches : Seq[Seq[XEvent]]
) extends Features(Xor(name))

case class TaskFeatures( 
  name : String,
  matches : Seq[Seq[XEvent]]
) extends Features(Task(name))

case class Sequence(name : String) extends FeatureType { def hasDuration = true }
case class Loop(name : String) extends FeatureType { def hasDuration = true }
case class Task(name : String) extends FeatureType { def hasDuration = false }
case class And(name : String) extends FeatureType { def hasDuration = true }
case class Xor(name : String) extends FeatureType { def hasDuration = false }