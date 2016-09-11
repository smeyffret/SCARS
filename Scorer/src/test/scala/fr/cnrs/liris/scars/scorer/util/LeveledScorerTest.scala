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
class LeveledScorerTest extends FunSuite with ShouldMatchers {

  implicit val database = new MockDatabase()

  val values: Seq[Double] = List(1,2,3,4,5)

  test("Extreme values should return leveled score") {
    leveledScore(new FixedScorer(1)) should be (Some(1))
    leveledScore(new FixedScorer(5)) should be (Some(5))
  }

  test("Leveled scores should return themself") {
    leveledScore(new FixedScorer(2)) should be (Some(2))
    leveledScore(new FixedScorer(3)) should be (Some(3))
    leveledScore(new FixedScorer(4)) should be (Some(4))
  }

  test("An upper value should return immediate lower leveled score") {
    leveledScore(new FixedScorer(2.2)) should be (Some(2))
  }

  test("A lower value should return immediate upper leveled score") {
    leveledScore(new FixedScorer(1.9)) should be (Some(2))
  }

  test("A middle value should return the upper leveled score") {
    leveledScore(new FixedScorer(2.5)) should be (Some(3))
    leveledScore(new FixedScorer(3.5)) should be (Some(4))
  }

  test("A none value should return None") {
    leveledScore(NoneScorer) should be (None)
  }
  
  def leveledScore(scorer: Scorer) = new LeveledScorer(scorer, values).score(1,1).map(_.value)

}