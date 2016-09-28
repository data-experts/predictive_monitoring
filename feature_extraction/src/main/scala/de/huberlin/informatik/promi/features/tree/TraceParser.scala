package de.huberlin.informatik.promi.features.tree

import org.deckfour.xes.model.XTrace
import de.huberlin.informatik.promi.features.Features
import org.processmining.processtree
import processtree._
import scala.collection.JavaConversions._
import de.huberlin.informatik.promi.xes.XESImplicits._
import java.util.Date
import scala.collection.immutable.BitSet

private[tree] class TraceParser(trace: XTrace, tree: ProcessTree) {

  import TraceParser._

  val indexedTrace = trace.zipWithIndex

  def parse(): ParseTree = {
    def parse(subtree: processtree.Node): ParseTree =
      subtree match {
        case task: processtree.Task if task.getName == "tau" =>
          ParseLeaf(task, Seq(TauMatch))
        case task: processtree.Task =>
          val matches = indexedTrace.
            filter(_._1.getSimpleEventName() == task.getName).
            map(p => Match(p._2))
          ParseLeaf(task, matches)

        case block: processtree.Block =>
          val children = block.getChildren.map(parse(_))
          val matches = children.map(_.matches)
          block match {
            case _: Block.Seq => ParseNode(block, combineSequence(matches), children)
            case _: Block.XorLoop => ParseNode(block, combineLoop(matches), children)
            case _: Block.Xor => ParseNode(block, combineXor(matches), children)
            case _: Block.And => ParseNode(block, combineAnd(matches), children)
          }
      }
    parse(tree.getRoot)
  }

}

trait SubtreeMatch {
  def min: Int
  def max: Int
  def strictlyBefore(other: SubtreeMatch): Boolean
  def strictlyAfter(other: SubtreeMatch): Boolean
}

/**
 * A match along with all indices (in the trace) belonging to this match
 */
case class Match(idx: BitSet, min: Int, max: Int) extends SubtreeMatch {
  def strictlyBefore(other: SubtreeMatch) = other match {
    case m: Match => max < m.min
    case _ => true
  }
  def strictlyAfter(other: SubtreeMatch) = other match {
    case m: Match => min > m.max
    case _ => true
  }

  override def toString = idx.mkString("[", ",", "]")
}

case object TauMatch extends SubtreeMatch {
  def strictlyBefore(other: SubtreeMatch) = true
  def strictlyAfter(other: SubtreeMatch) = true
  def min = Integer.MAX_VALUE
  def max = Integer.MIN_VALUE
}

object Match {
  def apply(i: Int*) = new Match(BitSet(i: _*), i.min, i.max)
}

sealed trait ParseTree {
  def matches: Seq[SubtreeMatch]
  def node: processtree.Node
  def fold[B](init: B)(f: (B, ParseTree) => B): B = {
    this match {
      case _: ParseLeaf => f(init, this)
      case node: ParseNode =>
        node.children.foldLeft(f(init, this))((acc, tree) => tree.fold(acc)(f))
    }
  }
}
case class ParseLeaf(node: processtree.Node, matches: Seq[SubtreeMatch]) extends ParseTree
case class ParseNode(node: processtree.Block, matches: Seq[SubtreeMatch], children: Seq[ParseTree]) extends ParseTree

private[tree] object TraceParser {

  def combineXor(matches: Seq[Seq[SubtreeMatch]]): Seq[SubtreeMatch] = matches.flatten

  def combineAnd(matches: Seq[Seq[SubtreeMatch]]): Seq[SubtreeMatch] = {
    val idx = matches.flatMap(_.map(_.max)).sorted
    (idx.foldLeft((Seq.empty[SubtreeMatch], matches)) {
      case ((res, tails), ix) => {
        val splits = tails.map(_.span { _.max <= ix })
        val curr = splits.map(_._1)
        if (curr.forall(!_.isEmpty)) {
          val currMatch = curr.map(_.maxBy(_.max))
          val retainTaus = curr.map(_.filter { _ == TauMatch })
          (res :+ combineMatches(currMatch : _*), 
              retainTaus.zip(splits.map(_._2)).map(p => p._1 ++ p._2))
        } else {
          (res, tails)
        }
      }
    })._1
  }

  def combineSequence(matches: Seq[Seq[SubtreeMatch]]): Seq[SubtreeMatch] = {
    if (matches.isEmpty) Seq()
    else if (matches.size == 1) matches.head
    else {
      val tails = combineSequence(matches.tail)
      def merge(s1 : Seq[SubtreeMatch], 
          s2: Seq[SubtreeMatch], 
          left : Seq[SubtreeMatch], 
          right : Seq[SubtreeMatch]) : Seq[SubtreeMatch] =
        (s1, s2) match {
          case (Nil, Nil) => Seq()
          case (_, Nil) => Seq(combineMatches(s1 ++ left ++ right :_*))
          case (Nil, _) => Seq(combineMatches(s2 ++ left ++ right :_*))
          case (x1 +: xs1, x2 +: xs2) =>
            if (x1.strictlyBefore(x2)) {
              if (right.isEmpty) merge(xs1, s2, x1 +: left, right)
              else combineMatches(left ++ right :_*) +: merge(xs1, s2, x1 +: Nil, Nil)
            }
            else {
              merge(s1, xs2, left, x2 +: right)
            }
      }
      merge(matches.head, combineSequence(matches.tail), Seq(), Seq())
    }
  }

  def combineLoop(matches: Seq[Seq[SubtreeMatch]]): Seq[SubtreeMatch] = {
    if (matches.isEmpty) Seq()
    else {
      val forward = matches.head
      val backwards = matches.tail.flatten
      (for (
        fs <- forward.tails;
        if (!fs.isEmpty);
        f = fs.head
      ) yield {
        val followers = backwards.filter { b => (f.strictlyBefore(b) && fs.tail.forall { _.strictlyAfter(b) }) }
        if (followers.isEmpty) f
        else combineMatches(f, followers.minBy(_.min))
      }).toSeq
    }
  }

  def combineMatches(matches: SubtreeMatch*): SubtreeMatch =
    matches.reduce(
      (_, _) match {
        case (TauMatch, m) => m
        case (m, TauMatch) => m
        case (Match(s1, min1, max1), Match(s2, min2, max2)) =>
          Match(s1 ++ s2, min1.min(min2), max1.max(max2))
      })
}