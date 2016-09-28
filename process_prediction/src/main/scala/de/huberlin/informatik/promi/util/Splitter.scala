package de.huberlin.informatik.promi.util

import org.deckfour.xes.model.XLog
import org.deckfour.xes.factory.XFactoryRegistry
import scala.collection.JavaConversions._
import com.typesafe.scalalogging.LazyLogging
import de.vandermeer.asciitable.v2._
import de.vandermeer.asciitable.v2.render._
import de.vandermeer.asciitable.v2.themes._
import org.deckfour.xes.extension.std.XLifecycleExtension
import de.huberlin.informatik.promi.Configuration
import java.io.File

/**
 * Helper class for splitting the set of log files into training, validation and test sets
 */
object Splitter extends LazyLogging {

  sealed trait Split

  case class AsInFiles(
    train: Int, valid: Int, test: Int) extends Split
    
  case class TrainingTest(
    trainFolder : String,
    testFolder : String
  ) extends Split
    
  def split(config: Configuration): (XLog, XLog, XLog) =
    config.split match {
      case AsInFiles(tr, v, te) => {
        splitByFiles(config.fileNames, tr, v, te, config)
      }
      case TrainingTest(trainFolder, testFolder) => {
        val trainFiles = new File(trainFolder).listFiles()
        val testFiles = new File(testFolder).listFiles()
        val protoLog = config.logParser.getLog(trainFiles.head.getPath)
        val trainLog = initLog(protoLog)
        val testLog = initLog(protoLog)
         
        for (file <- trainFiles) {
           val log = config.logParser.getLog(file.getPath)
           for (trace <- log; if config.instanceFilter(trace)) {
             trainLog.add(trace)
           }
        }
        for (file <- testFiles; if config.useTestSet) {
           val log = config.logParser.getLog(file.getPath)
           for (trace <- log; if config.instanceFilter(trace)) {
             testLog.add(trace)
           }
        }
        (trainLog, initLog(trainLog) /*empty validation set*/, testLog)
        
      }
    }

  private def splitByFiles(fileNames: Seq[String], tr: Int, v: Int, te: Int, config: Configuration): (XLog, XLog, XLog) = {
    val orderedNames = fileNames.sortBy { Configuration.getYear(_) }
    val (trNames, restNames) = orderedNames.splitAt(tr)
    val (vNames, testNames) = restNames.splitAt(v)
    require(testNames.size == te)

    logger.info("Training files: " + trNames.mkString(","))
    logger.info("Validation files: " + vNames.mkString(","))
    logger.info("Test files: " + testNames.mkString(","))

    def concatLog(files: Seq[String]) = {
      if (files.isEmpty) {
        logger.warn("List of files for split is empty")
        XFactoryRegistry.instance().currentDefault().createLog()
      } else {
        val logs = files.map(config.logParser.getLog(_))
        val concatLog = initLog(logs.head)
        logs.foreach(l => {
          for (trace <- l; event <- trace) {
            if (!event.getAttributes.contains(XLifecycleExtension.KEY_TRANSITION)) {
              event.getAttributes.put(XLifecycleExtension.KEY_TRANSITION, attrComplete)
            }
          }
          for (trace <- l; if config.instanceFilter(trace)) concatLog.add(trace)
        })
        concatLog
      }
    }

    val trainingLog = concatLog(trNames)
    val validationLog = concatLog(vNames)
    val testLog = if (config.useTestSet) concatLog(testNames) else initLog(trainingLog)

    (trainingLog, validationLog, testLog)
  }

  lazy val attrComplete = XFactoryRegistry.instance().currentDefault().
    createAttributeLiteral(
      XLifecycleExtension.KEY_TRANSITION,
      XLifecycleExtension.StandardModel.COMPLETE.toString,
      XLifecycleExtension.instance())

  /**
   * creates a copy of a log
   */
  private def initLog(inputLog: XLog) = XFactoryRegistry.instance().currentDefault().createLog(inputLog.getAttributes)

}