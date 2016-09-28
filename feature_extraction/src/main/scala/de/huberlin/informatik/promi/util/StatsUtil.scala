package de.huberlin.informatik.promi.util

object StatsUtil {
  
  import Numeric.Implicits._
  
  def mean[T : Numeric](vals : Seq[T]) : Double = vals match {
    case Seq() => 0.0
    case _ => vals.sum.toDouble / vals.length
  }
  
  def median[T : Numeric](vals : Seq[T]) : Double = vals match {
    case Seq() => 0.0
    case Seq(x) => x.toDouble
    case _ if vals.length % 2 == 0 => 
       (vals.sorted.apply((vals.length / 2)) + vals.sorted.apply((vals.length / 2) - 1)).toDouble / 2d
    case _ => vals.sorted.apply((vals.length / 2)).toDouble
  }
  
}