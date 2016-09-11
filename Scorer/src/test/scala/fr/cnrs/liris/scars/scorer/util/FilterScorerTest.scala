package fr.cnrs.liris.scars.scorer.util

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import fr.cnrs.liris.scars.api._
import fr.cnrs.liris.scars.api.Conversion._
import org.scalatest.mock.EasyMockSugar

/**
 * @author  Simon Meyffret
 * @version 0.1
 * @since   scala 2.8
 * Date: 9 déc. 2010
 * Time: 12:32:04
 */

@RunWith(classOf[JUnitRunner])
class FilterScorerTest extends FunSuite with ShouldMatchers {

  class TestScorer(score: Option[Double], var count: Int = 1) extends Scorer {
    def score(actor: Actor, item: Item, without: Set[Review]) = {
      if (count > 0){
        count -= 1
        score.map(Score(_))
      } else {
        throw new RuntimeException("this scorer shall not be called (%s)".format(score))
      }
    }
  }
  object ForbiddenScorer extends TestScorer(None, 0)

  def fixed(score: Int) = new TestScorer(Some(score))
  def twice(score: Int) = new TestScorer(Some(score), 2)
  def none = new TestScorer(None)
  def forbidden = ForbiddenScorer

  implicit val database = new MockDatabase()

  test("a FilterScorer needs at least 2 scorers") {
    intercept[RuntimeException] { compose() }
    intercept[RuntimeException] { compose(forbidden) }
  }

  test("NoneScorer should filter the rest") {
    compose(none, forbidden) should be(None)
  }

  test("NoneScorer should filter all the rest") {
    compose(none, forbidden, forbidden, forbidden) should be(None)
  }

  test("The last FixedScorer should return a score") {
    compose(fixed(1), fixed(2)) should be(Some(2))
  }

  test("The same FixedScorer should be called twice") {
    val scorer = twice(1)
    compose(scorer, scorer) should be(Some(1))
  }

  test("Any scorer after a valid FixedScorer should be called") {
    intercept[RuntimeException] { compose(fixed(1), forbidden) }
  }

  test("FixedScorer, NoneScorer and anything should be filtered") {
    compose(fixed(1), none, forbidden) should be(None)
  }

  def compose(scorer: TestScorer*) = try {
    new FilterScorer(scorer: _*).score(1, 1).map(_.value)
  } finally {
    scorer.map(_.count).toList should be (List.fill(scorer.size)(0))
  }
  
}