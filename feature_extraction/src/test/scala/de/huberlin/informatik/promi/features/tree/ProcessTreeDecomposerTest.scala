package de.huberlin.informatik.promi.features.tree

import org.deckfour.xes.in.XesXmlParser
import org.deckfour.xes.model.XTrace
import org.scalatest.Finders
import org.scalatest.FunSpec
import org.scalatest.Matchers

import de.huberlin.informatik.promi.features._

class ProcessTreeDecomposerTest extends FunSpec with Matchers {

  def getLog(fileName: String) = {
    val stream = getClass.getResourceAsStream(fileName)
    try {
      new XesXmlParser().parse(stream).get(0)
    } finally {
      stream.close()
    }
  }

  val sequenceLog = getLog("/example_sequence.xes"); require(sequenceLog.size() == 1)
  val simpleLog = getLog("/example_simple.xes"); require(simpleLog.size() == 1)
  val complexLog1 = getLog("/example_prom1.xes"); require(complexLog1.size() > 1)

  def traceAt(idx: Int*)(implicit trace: XTrace) = idx.map(trace.get(_))

  describe("A single sequential trace") {
    implicit val trace = sequenceLog.get(0)
    trace should have length 5
    val map = new ProcessTreeDecomposer(sequenceLog, ProcessTreeParams()).getFeatureMap()
    it("should produce a list of features") {
      map.listFeatures should matchPattern {
        case Seq(
          Sequence("Seq(A, B, C, D, E)"),
          Task("A"),
          Task("B"),
          Task("C"),
          Task("D"),
          Task("E")
          ) =>
      }
    }
    it("should produce a 2-level feature map") {
      val fMap = map.apply(trace)
      fMap should have size 6
      fMap should contain(SequenceFeatures("Seq(A, B, C, D, E)", Seq(traceAt(0,1,2,3,4))))
      fMap should contain(TaskFeatures("A", Seq(traceAt(0))))
      fMap should contain(TaskFeatures("B", Seq(traceAt(1))))
      fMap should contain(TaskFeatures("C", Seq(traceAt(2))))
      fMap should contain(TaskFeatures("D", Seq(traceAt(3))))
      fMap should contain(TaskFeatures("E", Seq(traceAt(4))))
    }
  }

  describe("A single trace with a loop") {
    //A - B - C - D - E - F - C - D - G
    implicit val trace = simpleLog.get(0)
    trace should have length 9
    val map = new ProcessTreeDecomposer(simpleLog, ProcessTreeParams()).getFeatureMap()
    it("should produce a list of features") {
      map.listFeatures should matchPattern {
        case Seq(
          Sequence("Seq(A, B, XorLoop(Seq(C, D), Seq(E, F), tau), G)"),
          Task("A"),
          Task("B"),
          Loop("XorLoop(Seq(C, D), Seq(E, F), tau)"),
          Sequence("Seq(C, D)"),
          Task("C"),
          Task("D"),
          Sequence("Seq(E, F)"),
          Task("E"),
          Task("F"),
          Task("G")
          ) =>
      }
    }
    it("should produce a multi-level feature map") {
      val fMap = map.apply(trace)
      fMap should have size 11
      fMap should contain(SequenceFeatures("Seq(A, B, XorLoop(Seq(C, D), Seq(E, F), tau), G)", Seq(traceAt(0 to 8 :_*))))
      fMap should contain(
          LoopFeatures("XorLoop(Seq(C, D), Seq(E, F), tau)", Seq(traceAt(2 to 5 :_*), traceAt(6 to 7 :_*))))
      fMap should contain(SequenceFeatures("Seq(C, D)", Seq(traceAt(2,3), traceAt(6,7))))
      fMap should contain(SequenceFeatures("Seq(E, F)", Seq(traceAt(4,5))))
      fMap should contain(TaskFeatures("A", Seq(traceAt(0))))
      fMap should contain(TaskFeatures("B", Seq(traceAt(1))))
      fMap should contain(TaskFeatures("C", Seq(traceAt(2), traceAt(6))))
      fMap should contain(TaskFeatures("D", Seq(traceAt(3), traceAt(7))))
      fMap should contain(TaskFeatures("E", Seq(traceAt(4))))
      fMap should contain(TaskFeatures("F", Seq(traceAt(5))))
      fMap should contain(TaskFeatures("G", Seq(traceAt(8))))
    }
  }

  describe("An event log") {
    val map = new ProcessTreeDecomposer(complexLog1, ProcessTreeParams()).getFeatureMap()
    it("should produce a list of features") {
      map.listFeatures should matchPattern {
        case Seq(
          Sequence(_),
          Task("A"),
          Loop(_),
          Sequence(_),
          And(_),
          Task("C"),
          Xor(_),
          Task("B"),
          Task("F"),
          Task("D"),
          Task("E"),
          Xor(_),
          Task("G"),
          Task("H")
          ) =>
      }
    }
    it("should produce a multilevel-level sparse feature map for trace 5") {
      implicit val trace = complexLog1.get(5)
      trace should have length 5
      val fMap = map.apply(trace)
      fMap should have size 14
      println(fMap mkString "\n")
      fMap should contain(SequenceFeatures("Seq(A, XorLoop(Seq(And(C, Xor(B, F)), D), E, tau), Xor(G, H))",
        Seq(traceAt(0 to 4 : _*))))
      fMap should contain(LoopFeatures("XorLoop(Seq(And(C, Xor(B, F)), D), E, tau)", Seq(traceAt(1,2,3))))
      fMap should contain(XorFeatures("Xor(G, H)", Seq(traceAt(4))))
      fMap should contain(SequenceFeatures("Seq(And(C, Xor(B, F)), D)", Seq(traceAt(1,2,3))))
      fMap should contain(AndFeatures("And(C, Xor(B, F))", Seq(traceAt(1,2))))
      fMap should contain(XorFeatures("Xor(B, F)", Seq(traceAt(2))))
      fMap should contain(TaskFeatures("A", Seq(traceAt(0))))
      fMap should contain(TaskFeatures("B", Seq()))
      fMap should contain(TaskFeatures("C", Seq(traceAt(1))))
      fMap should contain(TaskFeatures("D", Seq(traceAt(3))))
      fMap should contain(TaskFeatures("E", Seq()))
      fMap should contain(TaskFeatures("F", Seq(traceAt(2))))
      fMap should contain(TaskFeatures("G", Seq()))
      fMap should contain(TaskFeatures("H", Seq(traceAt(4))))
    }
  }

}