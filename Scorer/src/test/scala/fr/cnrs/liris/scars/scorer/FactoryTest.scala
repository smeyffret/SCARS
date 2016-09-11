package fr.cnrs.liris.scars.scorer

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
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
class FactoryTest extends FunSuite with ShouldMatchers {

  val Score = 0
  def fixed(score: Double) = new ScorerCounter(Some(score))
  def none = new ScorerCounter(Some(Score), 0)

  implicit val database = new MockDatabase()
  
  test("a factory should build a new scorer every time") {
    ScorerCounter.reset
    ScorerCounter.count should be (0)
    val factory = Factory(fixed(1))
    ScorerCounter.count should be (1) // initiale instance
    (1 to 3).map(_ => factory()).foreach { scorer =>
      score(scorer)(Some(1))
      intercept[RuntimeException] { score(scorer)(None) }
    }
    ScorerCounter.count should be (3)
  }

  test("compose scorers return a new scorer") {
    ScorerCounter.reset
    ScorerCounter.count should be (0)
    val factory = Factory(fixed(1)).compose{f => 
      fixed(2)
    }
    ScorerCounter.count should be (2) // initiales instances
    (1 to 3).foreach { _ =>
      val scorer = factory()
      score(scorer)(Some(2))
      intercept[RuntimeException] { score(scorer)(None) }
    }
    ScorerCounter.count should be (6)
  }

  def score(scorer: Scorer)(score: Option[Double]) = {
    scorer.score(1, 1).map(_.value) should be (score)
  }
  
}