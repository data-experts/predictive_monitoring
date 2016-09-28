package de.huberlin.informatik.promi.util

import org.deckfour.xes.model.XLog
import org.deckfour.xes.in.XesXmlParser
import scala.collection.JavaConversions._
import com.typesafe.scalalogging.LazyLogging
import org.deckfour.xes.in.XesXmlGZIPParser

/**
 * Simple log parser that deserializes an XES file
 */
class LogParser extends LazyLogging {

  def getLog(file: String): XLog = {
    val parser = if (file.endsWith(".gz")) new XesXmlGZIPParser else new XesXmlParser
    val logs = parser.parse(new java.io.File(file))
    require(logs.size() == 1, "exactly one log")
    val log = logs(0)
    logger.info("Finished deserializing " + file)
    log
  }
}

/**
 * Log parser that caches the parsed XES files<p>
 * Useful if a log is used multiple times, e.g., when comparing different learning configurations
 */
class CachedLogParser(visitor : LogVisitor = NullVisitor) extends LogParser {
  
  val cache = scala.collection.mutable.Map[String, XLog]()
  
  override def getLog(file: String): XLog = {
    if (cache.contains(file)) cache(file)
    else { 
      val log = super.getLog(file)
      visitor.visit(log)
      cache += (file -> log)
      log
    }
  }
  
}

trait LogVisitor {
  def visit(log : XLog) : Unit
}

object NullVisitor extends LogVisitor {
  def visit(log : XLog) : Unit = ()
}