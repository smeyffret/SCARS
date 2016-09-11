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
class StatsSimpleTest extends FunSuite with ShouldMatchers {

  val scores = List((1,1,1.0,None), (1,2,1.0,Some(2.0)), (2,2,2.0,Some(1.0)))
  import Stats._
  val statsBuilder = new StatsBuilder(scores, Nil, mae, rmse, wae, rwse, srcc)
  val stats = statsBuilder.build()

  test("Two computed scores should yield a MAE") {
    stats.get(mae).get should be (1.0)
  }

  test("Two computed scores should yield a RMSE") {
    stats.get(rmse).get should be (1.0)
  }

  test("Two computed scores should yield a WAE") {
    //stats.wae should be (2.0)
  }

  test("Two computed scores should yield a RWSE") {
    //stats.rwse should be (2.0)
  }

  test("Two computed scores should yield a SRRC") {
    val list = scores.filter(_._4.isDefined)
    val srcc = statsBuilder.spearmanRankCorrelationCoefficient(statsBuilder.getScores(list))
    srcc should be (-1.0) // croit en sens inverse
  }

  test("Two database should yield a mean") {
    val item1 = 1
    val item2 = 2
    statsBuilder.mean(item1) should be (Some(1.0))
    statsBuilder.mean(item2) should be (Some(1.5))
  }

}
