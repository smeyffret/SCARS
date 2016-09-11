package fr.cnrs.liris.scars.scorer.util

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import fr.cnrs.liris.scars.scorer._
import fr.cnrs.liris.scars.api._
import fr.cnrs.liris.scars.api.Conversion._

/**
 * @author  Simon Meyffret
 * @version 0.1
 * @since   scala 2.8
 * Date: 9 déc. 2010
 * Time: 12:32:04
 */

@RunWith(classOf[JUnitRunner])
class CompositeScorerTest extends FunSuite with ShouldMatchers {

  def fixed(score: Int) = new TestScorer(Some(score))
  def none = new TestScorer(None)
  def forbidden = ForbiddenScorer

  implicit val database = new MockDatabase()

  test("A forbidden scorer should produce an exception") {
    intercept[RuntimeException] { compose(forbidden) }
  }

  test("A test_scorer should produce an exception if called twice") {
    val scorer = none
    intercept[RuntimeException] { compose(scorer, scorer) }
  }

  test("An empty CompositeScorer should return None") {
    compose() should be(None)
  }

  test("A one FixedScorer CompositeScorer should return a value") {
    compose(fixed(1)) should be(Some(1))
  }

  test("The same FixedScorer CompositeScorer should not be called twice (the first result is enough)") {
    val scorer = fixed(1)
    compose(scorer, scorer) should be(Some(1))
  }

  test("The first FixedScorer CompositeScorer should return a value") {
    compose(fixed(1), forbidden) should be(Some(1))
  }

  test("A one NoneScorer CompositeScorer should return None") {
    compose(none) should be(None)
  }

  test("NoneScorer and FixedScorer CompositeScorer should return a value") {
    compose(none, fixed(1)) should be(Some(1))
  }

  test("NoneScorer, FixedScorer and NoneScorer CompositeScorer should return a value") {
    compose(none, fixed(1), forbidden) should be(Some(1))
  }

  def compose(scorer: TestScorer*) = try {
    new CompositeScorer(scorer: _*).score(1, 1).map(_.value)
  } finally {
    scorer.map(_.count).toList should be (List.fill(scorer.size)(0))
  }
  
}