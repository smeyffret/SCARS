package fr.cnrs.liris.scars.api

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import fr.cnrs.liris.scars.api.Conversion._

/**
 * @author  Simon Meyffret
 * @version 0.0.1
 * @since   scala 2.8
 * Date: 12 avr. 2011
 * Time: 17:11:43
 */

@RunWith(classOf[JUnitRunner])
class ScorerTest extends FunSuite with ShouldMatchers {
  
  implicit val db = new MockDatabase()
  
  db(1, 1) = 1.1
  db(1, 2) = 1.2
  db(1, 3) = 1.3
  
  val scorer = NoneScorer

  test("A scorer return existing ratings with empty without"){
    val without = Set[Review]()
    (1 to 3).foreach { idItem =>
      val rating: Score = 1 + idItem / 10.
      scorer.rating(1, idItem, without) should be (Some(rating))
    }
  }

  test("A scorer return no ratings without it"){
    (1 to 3).foreach { idItem =>
      val without = 1.review(idItem).toSet
      scorer.rating(1, idItem, without) should be (None)
    }
  }

  test("A scorer return existing ratings without other reviews"){
    (1 to 3).foreach { idItem =>
      val without = db.reviews.toSet -- 1.review(idItem).toSet
      val rating: Score = 1 + idItem / 10.
      scorer.rating(1, idItem, without) should be (Some(rating))
    }
  }

  test("A scorer return no ratings without all reviews"){
    (1 to 3).foreach { idItem =>
      val without = db.reviews.toSet
      scorer.rating(1, idItem, without) should be (None)
    }
  }

}
