package fr.cnrs.liris.scars.stats

import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import fr.cnrs.liris.scars.math.Math

/**
 * @author  Simon Meyffret
 * @version 0.0.1
 * @since   scala 2.8
 * Date: 12/12/10
 * Time: 19:08
 */

@RunWith(classOf[JUnitRunner])
class StatsSRRCTest extends FunSuite with ShouldMatchers {

  import Stats._

  // exemple ici : http://www.statistics4u.info/fundstat_eng/cc_corr_spearman.html
  val multipleScores = List((1,0,1.0,None),
    (1,1,1.0,Some(2.0)), (1,2,2.0,Some(3.0)), (1,3,4.0,Some(5.0)), (1,4,5.0,Some(4.0)), (1,5,2.0,Some(2.0)),
    (1,6,2.0,Some(2.0)), (1,7,4.0,Some(3.0)), (1,8,3.0,Some(4.0)), (1,9,1.0,Some(3.0)), (1,10,4.0,Some(2.0)),
  (1,11,3.0,None))
  // eval
  // triés : 1, 1, 2, 2, 2, 3, 4, 4, 4, 5
  // rank  : 1, 2, 3, 4, 5, 6, 7, 8, 9, 10
  // ties  : 1.5 , 4      , 6, 8      , 10    taille -> 5
  //
  // comp
  // triés : 2, 2, 2, 2, 3, 3, 3, 4, 4, 5
  // rank  : 1, 2, 3, 4, 5, 6, 7, 8, 9, 10
  // ties  : 2.5       , 6      , 8.5 , 10    taille -> 4

  val sameOrderScores = List((1,1,1.0,None),
    (1,2,1.0,Some(2.0)), (1,2,2.0,Some(3.0)), (1,3,3.0,Some(4.0)), (1,3,3.0,Some(4.0)), (1,3,1.0,Some(3.0)),
  (1,3,3.0,None))

  val otherOrderScores = List((1,1,1.0,None),
    (1,2,3.0,Some(2.0)), (1,2,2.0,Some(3.0)), (1,3,3.0,Some(1.0)), (1,3,3.0,Some(1.0)), (1,3,4.0,Some(0.5)),
  (1,3,3.0,None))

  test("Stats should compute an eval rank map") {
    val stats = new StatsBuilder(multipleScores, Nil)
    val evalValues = stats.definedValues.map(_._3)
    val evalRankMap = Math.rankMap(evalValues)
    evalRankMap should have size (5) // 5 valeurs différentes
    evalRankMap(1.0) should be (1.5)
    evalRankMap(2.0) should be (4.0)
    evalRankMap(3.0) should be (6.0)
    evalRankMap(4.0) should be (8.0)
    evalRankMap(5.0) should be (10.0)
  }

  test("Stats should compute a computed rank map") {
    val stats = new StatsBuilder(multipleScores, Nil)
    val computedValues = stats.definedValues.flatMap(_._4)
    val computedRankMap = Math.rankMap(computedValues)
    computedRankMap should have size (4) // 4 valeurs différentes
    computedRankMap.contains(1.0) should be (false)
    computedRankMap(2.0) should be (2.5)
    computedRankMap(3.0) should be (6.0)
    computedRankMap(4.0) should be (8.5)
    computedRankMap(5.0) should be (10.0)
  }

  test("Computed specific scores should yield a specific SRRC") {
    //val stats = Stats(multipleScores, Stats.srcc)
    val srcc = Math.spearmanCorrelationCoefficient(convert(multipleScores)).get
    srcc should be (0.5364 plusOrMinus 0.001)
  }

  test("Same order computed scores should yield a positive SRRC") {
    val stats = Stats(sameOrderScores, Stats.srcc, Stats.asrcc)
    stats.get(srcc).get should be > (0.0)
    stats.get(asrcc).get should be > (0.0)
  }

  test("Other order computed scores should yield a negative SRRC") {
    val stats = Stats(otherOrderScores, Stats.srcc, Stats.asrcc)
    stats.get(srcc).get should be < (0.0) // croit en sens inverse
    stats.get(asrcc).get should be < (0.0) // croit en sens inverse
  }

  def convert(list: List[(Any,Any,Double,Option[Double])]) = {
    val (ratings, scores) = list.collect{
      case (_, _, rating, Some(score)) => (Some(rating), Some(score))
    }.unzip
    val ratingsRank = Math.valuesToRanks(ratings)
    val scoresRank = Math.valuesToRanks(scores)
    (ratingsRank zip scoresRank) collect {
      case (Some(rating), Some(score)) => (rating, score)
    }
  }

}
