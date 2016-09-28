package de.huberlin.informatik.promi.map

import org.deckfour.xes.model.XTrace

object InstanceFilters {
  
  type Filter = XTrace => Boolean
  
  val all : Filter = _ => true
  
  def and(f1 : Filter, f2 : Filter) : Filter = t => f1(t) && f2(t)
  
  def or(f1 : Filter, f2 : Filter) : Filter = t => f1(t) || f2(t)
  
}