package de.huberlin.informatik.promi.visualize

import de.vandermeer.asciitable.v2._
import de.vandermeer.asciitable.v2.render._
import de.vandermeer.asciitable.v2.themes._
import com.typesafe.scalalogging.LazyLogging

/**
 * Helper class for printing ASCII tables
 */
object Table extends LazyLogging {
  
  def printTable(title : String, tableFunc : V2_AsciiTable => Unit, width : Int = 56, debug : Boolean = false, trace : Boolean = false) : Unit = {
    val at = new V2_AsciiTable()
    
    tableFunc(at)
    
    val rend = new V2_AsciiTableRenderer();
    rend.setTheme(V2_E_TableThemes.UTF_LIGHT.get());
    rend.setWidth(new WidthAbsoluteEven(width));
    
    val table = rend.render(at)
    
    if (trace) logger.trace(title + "\n\n" + table)
    else if (debug) logger.debug(title + "\n\n" + table)
    else logger.info(title + "\n\n" + table)
  }
  
}