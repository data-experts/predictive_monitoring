package de.huberlin.informatik.promi

import map._
import scala.collection.JavaConversions._
import weka.core._
import weka.filters.{ Filter => WekaFilter }
import weka.filters.unsupervised.attribute.Standardize
import org.deckfour.xes.model.XTrace
import weka.filters.AllFilter
import de.huberlin.informatik.promi.util._
import de.huberlin.informatik.promi.features.wekamap._
import Splitter._
import java.util.Locale

object Configuration {
  
  Locale.setDefault(Locale.US)
  
   def getYear(fileName: String): Int = {
    val split = fileName.split('.')
    val name = split(split.length - 2)
    name.substring(name.length() - 4, name.length()).toInt
  }

  def folder(name: String) = new java.io.File(name).listFiles().map(_.getAbsolutePath).toSeq
}

import Configuration._
import weka.filters.MultiFilter
import java.util.Locale

/**
 * Configuration for a particular learning scheme<p>
 * Contains all moving parts that can be easily swapped by invoking the copy method
 */
case class Configuration(
    fileNames: Seq[String],
    split: Split,
    filters: Seq[() => WekaFilter] = Seq(() => new AllFilter),
    instanceFilter : XTrace => Boolean = _ => true,
    kFoldCV: scala.Option[Int] = None,
    useTestSet : Boolean = true,
    features: Seq[CaseFeatures],
    output: Output,
    label : scala.Option[Output] = None,
    prefix : Prefix,
    classifier: scala.Option[Classifier],
    gridSearch : scala.Option[GridSearch] = None,
    weightDecay: Double = 0.0,
    positiveMultiplier : Double = 1.0,
    inductiveNoiseThreshold : Float = 0.0f,
    logParser : LogParser = new CachedLogParser(),
    ignoreAtts : Seq[String]
) {
  def wekaFilter = {
    val f = new MultiFilter
    f.setFilters(filters.map(_.apply()).toArray)
    f
  }
  
  def classIndex = if (output.isOneClass) 0 else 1
}