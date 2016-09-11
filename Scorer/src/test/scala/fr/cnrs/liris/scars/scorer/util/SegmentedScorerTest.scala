package fr.cnrs.liris.scars.scorer.util

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import fr.cnrs.liris.scars.api._
import fr.cnrs.liris.scars.api.Conversion._

/**
 * @author  Simon Meyffret
 * @version 0.0.1
 * @since   scala 2.8
 * Date: 9 déc. 2010
 * Time: 12:32:04
 */

@RunWith(classOf[JUnitRunner])
class SegmentedScorerTest extends FunSuite with ShouldMatchers {

  // Pour info, les segments sont : 1 - 1,8 - 2,6 - 3,4 - 4,2 - 5
  // avec comme valeur :              1     2     3     4     5

  implicit val database = new MockDatabase()

  val values: Seq[Double] = List(1,2,3,4,5)

  test("Extreme values should return segmented score") {
    segmentedScore(new FixedScorer(1)) should be (Some(1))
    segmentedScore(new FixedScorer(5)) should be (Some(5))
  }

  test("Upper class scores should return the class") {
    segmentedScore(new FixedScorer(1.8)) should be (Some(1))
    segmentedScore(new FixedScorer(2.6)) should be (Some(2))
    segmentedScore(new FixedScorer(3.4)) should be (Some(3))
    segmentedScore(new FixedScorer(4.2)) should be (Some(4))
  }

  test("Upper class scores + delta should return the next class") {
    segmentedScore(new FixedScorer(1.8 + 0.0001)) should be (Some(2))
    segmentedScore(new FixedScorer(2.6 + 0.0001)) should be (Some(3))
    segmentedScore(new FixedScorer(3.4 + 0.0001)) should be (Some(4))
    segmentedScore(new FixedScorer(4.2 + 0.0001)) should be (Some(5))
  }

  test("Inner class scores should return the class") {
    segmentedScore(new FixedScorer(1.3)) should be (Some(1))
    segmentedScore(new FixedScorer(2))   should be (Some(2))
    segmentedScore(new FixedScorer(3.1)) should be (Some(3))
    segmentedScore(new FixedScorer(3.9)) should be (Some(4))
  }

  test("A none value should return None") {
    segmentedScore(NoneScorer) should be (None)
  }
  
  def segmentedScore(scorer: Scorer) = new SegmentedScorer(scorer, values).score(1,1).map(_.value)

}
