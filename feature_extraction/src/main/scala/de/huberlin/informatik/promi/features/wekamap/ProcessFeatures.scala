package de.huberlin.informatik.promi.features.wekamap

import org.deckfour.xes.model.XTrace
import java.time.ZoneOffset
import com.typesafe.scalalogging.LazyLogging
import de.huberlin.informatik.promi.features._
import weka.core._
import de.huberlin.informatik.promi.util.StatsUtil
import java.time.Duration
import scala.collection.JavaConversions._
import de.huberlin.informatik.promi.xes.XESImplicits._
import de.huberlin.informatik.promi.features.Features
import weka.core.Instances
import de.huberlin.informatik.promi.xes.XESImplicits

/**
 * Profiles, e.g., which types of features are to be included
 */
sealed trait Profile
case object Count extends Profile
case object MinDate extends Profile
case object MaxDate extends Profile
case object DiffDate extends Profile
case object MinDuration extends Profile
case object MaxDuration extends Profile
case object MeanDuration extends Profile
case object SumDuration extends Profile

abstract class ProcessFeatures extends CaseFeatures with LazyLogging {
  
  def profile: Seq[Profile]
  
  val normalizeDurations: Boolean
  val durationTimescale: java.time.Duration => Double
  val dateTimescale: java.time.LocalDateTime => Double
  val imputationValue: scala.Option[Double]
  val prefix: String
  val ignoreLeaves : Boolean
  
  var map: scala.Option[FeatureMap] = None
 
  override def clear() = {
    super.clear
    map = None
  }
  
  def getFeatureMap : FeatureMap
  
  def getAttributes(): Seq[Attribute] = {
    map = Some(getFeatureMap)
    val features = map.get.listFeatures
    //logger.trace("Process tree features:\n" + features.mkString("\n"))
    features.flatMap(f =>
      f match {
        case Task(_) if ignoreLeaves => Seq()
        case _ if !f.hasDuration =>
          getAttribute(Count, f) ++
            getAttribute(MinDate, f) ++
            getAttribute(MaxDate, f) ++
            getAttribute(DiffDate, f)
        case _ =>
          getAttribute(Count, f) ++
            getAttribute(MinDate, f) ++
            getAttribute(MaxDate, f) ++
            getAttribute(DiffDate, f) ++
            getAttribute(MinDuration, f) ++
            getAttribute(MaxDuration, f) ++
            getAttribute(MeanDuration, f) ++
            getAttribute(SumDuration, f)
      })
  }

  def getAttribute(p: Profile, f: FeatureType): Seq[Attribute] = {
    if (!profile.contains(p)) Seq()
    else
      Seq(p match {
        case Count => new Attribute(getName(f))
        case MinDate => new Attribute(minDate(f))
        case MaxDate => new Attribute(maxDate(f))
        case DiffDate => new Attribute(diffDate(f))
        case MinDuration => new Attribute(minDur(f))
        case MaxDuration => new Attribute(maxDur(f))
        case MeanDuration => new Attribute(meanDur(f))
        case SumDuration => new Attribute(sumDur(f))
      })
  }

  def minDate(featureType: FeatureType) = "minDate-" + getName(featureType)
  def maxDate(featureType: FeatureType) = "maxDate-" + getName(featureType)
  def diffDate(featureType: FeatureType) = "diffDate-" + getName(featureType)
  def minDur(featureType: FeatureType) = "minDur-" + getName(featureType)
  def maxDur(featureType: FeatureType) = "maxDur-" + getName(featureType)
  def meanDur(featureType: FeatureType) = "meanDur-" + getName(featureType)
  def sumDur(featureType: FeatureType) = "sumDur-" + getName(featureType)
  def getName(featureType: FeatureType) =  prefix + "-" + featureType.name

  def addAttribute(trace: XTrace, inst: Instance, data: Instances): Unit = {
    val features = map.get.apply(trace)
    val maxTimespan = features.map(
      _.matches.map { m => durationTimescale(Duration.between(m.head.getTimestamp(), m.last.getTimestamp())) }).
      filter { !_.isEmpty }.map(_.max).max
    for (f <- features) {
      val attName = data.attribute(getName(f.featureType))
      val attMinDate = data.attribute(minDate(f.featureType))
      val attMaxDate = data.attribute(maxDate(f.featureType))
      val attDiffDate = data.attribute(diffDate(f.featureType))
      f match {
        case _ : TaskFeatures if ignoreLeaves =>
        case t => {
          if (profile.contains(Count)) inst.setValue(attName, t.matches.size)
          if (profile.contains(MinDate)) {
            if (t.matches.isEmpty && imputationValue.isDefined) inst.setValue(attMinDate, imputationValue.get)
            else if (!t.matches.isEmpty) inst.setValue(attMinDate, dateTimescale(t.matches.map(_.head.getTimestamp).min))
          }
          if (profile.contains(MaxDate)) {
            if (t.matches.isEmpty && imputationValue.isDefined) inst.setValue(attMaxDate, imputationValue.get)
            else if (!t.matches.isEmpty) inst.setValue(attMaxDate, dateTimescale(t.matches.map(_.last.getTimestamp).max))
          }
          if (profile.contains(DiffDate)) {
            if (t.matches.isEmpty && imputationValue.isDefined) inst.setValue(attDiffDate, imputationValue.get)
            else if (!t.matches.isEmpty)
              inst.setValue(attDiffDate,
                dateTimescale(t.matches.map(_.last.getTimestamp).max) - dateTimescale(t.matches.map(_.head.getTimestamp).min))
          }
          if (f.featureType.hasDuration) {
          val attMinDur = data.attribute(minDur(f.featureType))
          val attMaxDur = data.attribute(maxDur(f.featureType))
          val attMeanDur = data.attribute(meanDur(f.featureType))
          val attSumDur = data.attribute(sumDur(f.featureType))
          if (profile.contains(Count)) inst.setValue(attName, t.matches.size)
          if (profile.contains(MinDate)) {
            if (t.matches.isEmpty && imputationValue.isDefined) inst.setValue(attMinDate, imputationValue.get)
            else if (!t.matches.isEmpty) inst.setValue(attMinDate, dateTimescale(t.matches.map(_.head.getTimestamp).min))
          }
          if (profile.contains(MaxDate)) {
            if (t.matches.isEmpty && imputationValue.isDefined) inst.setValue(attMaxDate, imputationValue.get)
            else if (!t.matches.isEmpty) inst.setValue(attMaxDate, dateTimescale(t.matches.map(_.head.getTimestamp).max))
          }
          if (profile.contains(DiffDate)) {
            if (t.matches.isEmpty && imputationValue.isDefined) inst.setValue(attDiffDate, imputationValue.get)
            else if (!t.matches.isEmpty)
              inst.setValue(attDiffDate,
                durationTimescale(
                  Duration.between(t.matches.map(_.head.getTimestamp).min, t.matches.map(_.last.getTimestamp).max)))
          }

          if (t.matches.isEmpty && imputationValue.isDefined) {
            if (profile.contains(MinDuration))
              inst.setValue(attMinDur, imputationValue.get)
            if (profile.contains(MaxDuration))
              inst.setValue(attMaxDur, imputationValue.get)
            if (profile.contains(MeanDuration))
              inst.setValue(attMeanDur, imputationValue.get)
            if (profile.contains(SumDuration))
              inst.setValue(attSumDur, imputationValue.get)
          } else if (!t.matches.isEmpty) {
            val durations = t.matches.map { m => durationTimescale(Duration.between(m.head.getTimestamp(), m.last.getTimestamp())) }
            if (profile.contains(MinDuration))
              inst.setValue(attMinDur, normalizeDuration(durations.min, maxTimespan))
            if (profile.contains(MaxDuration))
              inst.setValue(attMaxDur, normalizeDuration(durations.max, maxTimespan))
            if (profile.contains(MeanDuration))
              inst.setValue(attMeanDur, normalizeDuration(StatsUtil.mean(durations), maxTimespan))
            if (profile.contains(SumDuration))
              inst.setValue(attSumDur, normalizeDuration(durations.sum, maxTimespan))
          }}
      }}
    }
  }

  private def normalizeDuration(dur: Double, max: Double): Double = {
    if (!normalizeDurations) dur
    else if (max == 0) 0
    else dur / max
  }
  
}