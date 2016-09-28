package de.huberlin.informatik.promi.features.tree

import org.deckfour.xes.model.XLog
import org.deckfour.xes.model.XLog
import org.deckfour.xes.model.XTrace
import org.processmining.plugins.InductiveMiner.mining.MiningParametersIMf
import org.processmining.plugins.InductiveMiner.plugins.IMProcessTree
import org.processmining.processtree.ProcessTree

import de.huberlin.informatik.promi._
import de.huberlin.informatik.promi.features._
import xes.XESImplicits._
import java.util.Date
import com.typesafe.scalalogging.LazyLogging
import org.deckfour.xes.model.XEvent

class ProcessTreeDecomposer(log: XLog, params: ProcessTreeParams) extends LazyLogging  {

  def getFeatureMap(): FeatureMap = {
    new ProcessTreeFeatureMap(getProcessTree)
  }

  def getProcessTree: ProcessTree = {
    val miningParams = new MiningParametersIMf
    miningParams.setNoiseThreshold(params.noiseThreshold)
    logger.trace("Start mining a process tree")
    val tree: ProcessTree = IMProcessTree.mineProcessTree(log, miningParams)
    logger.trace("Finished mining a process tree")
    tree
  }

}

case class ProcessTreeParams(
  noiseThreshold: Float = 0.0f)

class ProcessTreeFeatureMap(tree: ProcessTree) extends FeatureMap {

  import org.processmining.processtree
  import org.processmining.processtree.Block
  import scala.collection.JavaConversions._

  def listFeatures(): Seq[FeatureType] = listFeatures(tree.getRoot)

  def apply(trace: XTrace): Seq[Features] = {
    val parseTree = new TraceParser(trace, tree).parse()
     parseTree.fold(Seq.empty[Features]){
      case (map, subTree) => map ++ getFeatures(trace, subTree)
    }
  }

  private def listFeatures(subtree: processtree.Node): Seq[FeatureType] = {
    def getChildren(node : processtree.Block) = node.getChildren.flatMap(listFeatures(_))
    subtree match {
      case task: processtree.Task if task.getName == "tau" => Seq()
      case task: processtree.Task => Seq(Task(task.toString))
      case seq: Block.Seq =>
        Sequence(seq.toString) +: getChildren(seq)
      case and : Block.And =>
        And(and.toString) +: getChildren(and)
      case xor : Block.Xor =>
        Xor(xor.toString) +: getChildren(xor)
      case xorLoop: Block.XorLoop =>
        Loop(xorLoop.toString) +: getChildren(xorLoop)
    }
  }
  
  def getFeatures(trace : XTrace, tree: ParseTree) : Seq[Features] = {
    val events = getEvents(trace, tree.matches)
    tree.node match {
      case task: processtree.Task if task.getName == "tau" => Seq()
      case node : processtree.Task => Seq(TaskFeatures(node.toString, events))
      case node : Block.Seq => Seq(SequenceFeatures(node.toString, events))
      case node : Block.XorLoop => Seq(LoopFeatures(node.toString, events))
      case node : Block.And => Seq(AndFeatures(node.toString, events))
      case node : Block.Xor => Seq(XorFeatures(node.toString, events))
    }
  }

  def getEvents(trace : XTrace, matches: Seq[SubtreeMatch]) : Seq[Seq[XEvent]] =
    matches.toArray.map {
      case m : Match => m.idx.toSeq.sorted.map(trace(_))
      case TauMatch => Seq() 
   }.filter(!_.isEmpty)
}