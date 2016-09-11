package fr.cnrs.liris.scars.scorer.global

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import fr.cnrs.liris.scars.api._
import fr.cnrs.liris.scars.api.Conversion._
import fr.cnrs.liris.scars.test.Conversion._

/**
 * @author  Simon Meyffret
 * @version 0.0.1
 * @since   scala 2.8
 * Date: 9 déc. 2010
 * Time: 12:32:04
 */

@RunWith(classOf[JUnitRunner])
class GlobalMeanSimpleTest extends FunSuite with ShouldMatchers {

  implicit val database = new MockDatabase()

  database(1, 1) = 0.5
  database(2, 1) = 0.1; database(2, 2) = 0.2; database(2, 3) = 0.3
  database(3, 1) = 0.0; database(3, 2) = 1.0
  database(4, 1) = 0.5; database(4, 2) = 0.5; database(4, 3) = 0.1
  database(5, 1) = 0.1; database(5, 2) = 0.2; database(5, 3) = 0.2

  val scorer = new GlobalMean()

  test("A global mean scorer return existing scores with empty without") {
    score(1, 0.24)
    score(2, 0.475)
    score(3, 0.2)
  }

  test("A global mean scorer return existing scores with singleton without") {
    scoreWithout(1, 1, 0.175)
    scoreWithout(3, 1, 0.3)
    scoreWithout(5, 1, 0.275)
    
    scoreWithout(1, 2, 0.475)
    scoreWithout(3, 2, 0.3)
    
    scoreWithout(1, 3, 0.2)
    scoreWithout(3, 3, 0.2)
    scoreWithout(5, 3, 0.2)
  }

  def score(item: Item, result: Double) {
    database.actors.foreach { actor =>
      score(actor, item, result)
    }
  }
  
  def scoreWithout(actor: Actor, item: Item, result: Double) {
    val review = database.reviewsMap.get((actor, item)).map(Set(_)).getOrElse(Set.empty)
    score(actor, item, result, review)
  }
  
  def score(actor: Actor, item: Item, result: Double, without: Set[Review] = Set.empty) {
    scorer.score(actor, item, without).get.value should be(result plusOrMinus 0.000001)
  }

}
