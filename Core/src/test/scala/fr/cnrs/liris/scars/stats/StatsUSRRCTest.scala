package fr.cnrs.liris.scars.stats

import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSuite
import org.junit.runner.RunWith

/**
 * @author  Simon Meyffret
 * @version 0.0.1
 * @since   scala 2.8
 * Date: 12/12/10
 * Time: 19:08
 */

@RunWith(classOf[JUnitRunner])
class StatsUSRRCTest extends FunSuite with ShouldMatchers {

  import Stats._

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

  val sameOrderScores = List(
	  (1,1,1.0,None), (1,2,1.0,Some(2.0)), (1,3,2.0,Some(5.0)),
	  (2,1,5.0,Some(1.0)), (2,2,4.0,Some(3.0)), (2,3,2.0,Some(5.0)),
          (3,1,1.0,None), (3,2,2.0,Some(3.0)))

  val otherOrderScores = List((1,1,1.0,None),
    (1,2,3.0,Some(2.0)), (1,2,2.0,Some(3.0)), (1,3,3.0,Some(1.0)), (1,3,3.0,Some(1.0)), (1,3,4.0,Some(0.5)),
  (1,3,3.0,None))

//  test("Computed specific scores should yield a specific SRRC") {
//    val stats = Stats(dataset, multipleScores)
//    stats.usrrc should be (0.5364 plusOrMinus 0.001)
//  }

  test("Same order computed scores should yield a positive SRRC") {
    val stats = new StatsBuilder(sameOrderScores, Nil, Stats.asrcc)
    val scores = stats.getScores(sameOrderScores)
    stats.spearmanRankCorrelationCoefficient(scores.slice(1, 3)) should be (1)
    stats.spearmanRankCorrelationCoefficient(scores.slice(3, 6)) should be (-1)
    stats.build().get(asrcc).get should be ((1.0 - 1.0) / 2)
  }
//
//  test("Other order computed scores should yield a negative SRRC") {
//    val stats = Stats(dataset, otherOrderScores)
//    stats.usrrc should be < (0.0) // croit en sens inverse
//  }

}
