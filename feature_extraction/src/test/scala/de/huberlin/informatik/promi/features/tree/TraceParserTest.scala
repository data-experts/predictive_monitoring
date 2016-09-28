package de.huberlin.informatik.promi.features.tree

import org.scalatest._

class TraceParserTest extends FlatSpec with Matchers {

  "Sequence matches" should "be combined" in {

    val firstMatches = Seq(Match(0), Match(2), Match(4, 5))
    val secondMatches = Seq(Match(1), Match(6))

    val comb = TraceParser.combineSequence(Seq(firstMatches, secondMatches))

    comb should contain allOf (Match(0, 1), Match(2, 4, 5, 6))

  }

  "Sequence matches" should "be combined with maximum subsequences" in {

    val firstMatches = Seq(Match(0))
    val secondMatches = Seq(Match(1, 2), Match(1, 2, 3, 4))

    val comb = TraceParser.combineSequence(Seq(firstMatches, secondMatches))

    comb should have size 1
    comb should contain(Match(0, 1, 2, 3, 4))

  }

  "Tau matches" should "be combined" in {

    val firstMatches = Seq(TauMatch, Match(2))
    val secondMatches = Seq(Match(1), Match(3))

    val comb = TraceParser.combineSequence(Seq(firstMatches, secondMatches))

    comb should contain allOf (Match(1), Match(2, 3))

  }

  "Tau matches" should "be combined with and" in {

    val firstMatches = Seq(Match(1), TauMatch)
    val secondMatches = Seq(Match(2, 3), Match(4, 5, 6), TauMatch)

    val comb = TraceParser.combineAnd(Seq(firstMatches, secondMatches))

    comb should contain allOf (Match(1, 2, 3), Match(4, 5, 6))

  }

  "Loop matches" should "be combined" in {

    val firstMatches = Seq(Match(0), Match(3), Match(5), Match(7))
    val secondMatches = Seq(Match(1), Match(4))
    val thirdMatches = Seq(Match(2), Match(6))

    val comb = TraceParser.combineLoop(Seq(firstMatches, secondMatches, thirdMatches))

    comb should contain allOf (
      Match(0, 1), Match(3, 4), Match(5, 6), Match(7))

  }

  "Loop matches" should "be concatenated" in {

    val firstMatches = Seq(Match(1), Match(2))
    val secondMatches = Seq(Match(3))

    val comb = TraceParser.combineLoop(Seq(firstMatches, secondMatches))

    comb should contain allOf (Match(1), Match(2, 3))

  }

  "Loop matches" should "be combined in multiple stages" in {

    val firstMatches = Seq(Match(1), Match(3), Match(5), Match(7), Match(9))
    val secondMatches = Seq(Match(2), Match(8))
    val thirdMatches = Seq(Match(4), Match(6))

    val comb1 = TraceParser.combineLoop(Seq(firstMatches, secondMatches))
    val comb2 = TraceParser.combineLoop(Seq(comb1, thirdMatches))

    comb1 should contain allOf (
      Match(1, 2), Match(3), Match(5), Match(7, 8), Match(9))
    comb2 should contain allOf (
      Match(1, 2), Match(3, 4), Match(5, 6), Match(7, 8), Match(9))

  }

  "And matches" should "be combined" in {

    val firstMatches = Seq(Match(0), Match(4), Match(6), Match(7, 10))
    val secondMatches = Seq(Match(1), Match(3), Match(9))
    val thirdMatches = Seq(Match(2), Match(5), Match(8))

    val comb = TraceParser.combineAnd(Seq(firstMatches, secondMatches, thirdMatches))

    comb should contain allOf (Match(0, 1, 2), Match(3, 4, 5), Match(7, 8, 9, 10))

  }

}