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
class StatsEmptyTest extends FunSuite with ShouldMatchers {

  test("An empty result should yield no stat") {
    val stats = Stats(List.empty)
    val statsB = new StatsBuilder(List.empty, List.empty, Stats.values.toList: _*)
    Stats.values.foreach { metric =>
      stats.get(metric) should be(None)
    }
    statsB.mean(1) should be (None)
  }

  test("An empty result with existings reviews should yield no stat but some means") {
    val reviews = List( (1,1,1.),  (1,2,1.), (2,2,3.),  (1,3,2.), (2,3,3.), (3,3,4.) )
    val stats = Stats(List.empty)
    val statsB = new StatsBuilder(List.empty, reviews, Stats.values.toList: _*)
    Stats.values.foreach { metric =>
      stats.get(metric) should be(None)
    }
    (1 to 3).foreach{ item =>
      statsB.mean(item) should be (Some(item))
    }
  }

  {
    val scores = List((1,1,1.0,None), (1,2,1.0,None))
    val reviews: List[(Int, Int, Double)] = List((2,1,3.0), (2,2,3.0))
    val stats = Stats(scores)
    val statsB = new StatsBuilder(scores, reviews, Stats.values.toList: _*)
    
    test("No computed score should yield no stat") {
      Stats.values.foreach { metric =>
        stats.get(metric) should be(None)
      }
      val item1 = 1
      val item2 = 2
      statsB.mean(item1) should be (Some(2.0))
      statsB.mean(item2) should be (Some(2.0))
    }

  }

}
