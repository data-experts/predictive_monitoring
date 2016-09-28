package de.huberlin.informatik.promi.features.wekamap

import java.time.ZoneOffset

import scala.collection.JavaConversions._
import scala.collection.mutable

import org.deckfour.xes.model.XEvent
import org.deckfour.xes.model.XTrace

import de.huberlin.informatik.promi.xes.XESImplicits._
import de.huberlin.informatik.promi.features._

/**
 * Baseline implementation based on single events and bigrams
 */
case class SimpleProcessFeatures(
    val profile: Seq[Profile],
    val classifier: XEvent => String = _.getSimpleEventName(),
    val ignoreLeaves: Boolean = false,
    val normalizeDurations: Boolean = false,
    val durationTimescale: java.time.Duration => Double = _.toDays(),
    val dateTimescale: java.time.LocalDateTime => Double = _.toEpochSecond(ZoneOffset.UTC),
    val imputationValue: scala.Option[Double] = None,
    val simplePrefix: String = "",
    val useBigrams: Boolean = false) extends ProcessFeatures {

  val prefix = simplePrefix + "simple"

  val events = mutable.TreeSet[String]()
  val biGrams = mutable.TreeSet[(String, String)]()

  override def clear() = {
    super.clear
    events.clear
    biGrams.clear()
  }

  override def collect(trace: XTrace): Unit = {
    val mapped = trace.map(classifier)
    events ++= mapped
    if (useBigrams) biGrams ++= mapped.zip(mapped.tail)
  }

  def getFeatureMap: FeatureMap = new SimpleFeatureMap()

  class SimpleFeatureMap extends FeatureMap {

    def biGramName(biGram: (String, String)) = biGram._1 + "-" + biGram._2

    def listFeatures: Seq[FeatureType] = {
      events.toSeq.map(Task(_)) ++
        biGrams.toSeq.map { biGram => Sequence(biGramName(biGram)) }
    }

    def apply(trace: XTrace): Seq[Features] = {
      val eventOccurences = trace.groupBy(classifier).toMap.withDefaultValue(Seq())
      val singleEvents = events.toSeq.map(e => TaskFeatures(e, eventOccurences(e).map(Seq(_))))

      val biGramEvents =
        if (!useBigrams) Seq()
        else {
          val biGramOccurences =
            trace.zip(trace.tail).groupBy { case (e1, e2) => (classifier(e1), classifier(e2)) }.toMap.withDefaultValue(Seq())
          biGrams.toSeq.map(biGram =>
            SequenceFeatures(biGramName(biGram), biGramOccurences(biGram).map(b => Seq(b._1, b._2))))
        }

      singleEvents ++ biGramEvents
    }

  }

}