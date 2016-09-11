package fr.cnrs.liris.scars.scorer.global

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import fr.cnrs.liris.scars.api._
import fr.cnrs.liris.scars.api.Conversion._
import fr.cnrs.liris.scars.test.Conversion._
import fr.cnrs.liris.scars.math._

/**
 * @author  Simon Meyffret
 * @version 0.0.1
 * @since   scala 2.8
 * Date: 9 déc. 2010
 * Time: 12:32:04
 */

@RunWith(classOf[JUnitRunner])
class GlobalCFSimpleTest extends FunSuite with ShouldMatchers {

  implicit val db = new MockDatabase()

  db(1, 1) = 0.5
  db(2, 1) = 0.1; db(2, 2) = 0.2//; db(2, 3) = 0.3
  db(3, 1) = 0.8; db(3, 2) = 0.9; db(3, 3) = 0.85 // same average difference for items 1 and 2
//  database(4, 1) = 0.5; database(4, 2) = 0.5; database(4, 3) = 0.1
//  database(5, 1) = 0.1; database(5, 2) = 0.2; database(5, 3) = 0.2

  val scorer = new GlobalCF()
  def computeSim(db: Database) = SimilarityActor.sequential(db).foreach { case (actor, other, similarity) => 
    actor.asInstanceOf[MockActor].addSimilarity(other, similarity)
    other.asInstanceOf[MockActor].addSimilarity(actor, similarity)
  }
  computeSim(db)
  
  test("Similarity values should have been computed") {
    db.getActor(1).similars should be ('empty)
    db.getActor(2).similars should have size(1)
    db.getActor(2).similarity(3) should be(Some(1))
    db.getActor(3).similars should have size(1)
    db.getActor(3).similarity(2) should be(Some(1))
  }
  
  {
    val actor: Actor = 1
    db.items.foreach { item =>
      test("Cold start %s should not return score for %s".format(actor, item)) {
//        scorer.score(actor, item) should be(None)
      }
    }
  }

  {
    val actor: Actor = 2
    db.items.foreach { item =>
      test("%s should return a score for %s".format(actor, item)) {
//        scorer.score(actor, item) should not be (None)
      }
      actor.reviews.foreach{ review =>
        test("%s should not return score for %s without %s".format(actor, item, review)) {
//          scorer.score(actor, item, Set(review)) should be(None)
        }
      }
    }
  }

  {
    val actor: Actor = 3
    val item3: Item = 3
    val review3 = item3.reviews.head
    db.items.filterNot(_ == item3).foreach { item =>
      test("%s should return a score for %s".format(actor, item)) {
//        scorer.score(actor, item) should not be (None)
      }
      actor.reviews.filterNot(_ == review3).foreach { review =>
        test("%s should not return score for %s without %s".format(actor, item, review)) {
//          scorer.score(actor, item, Set(review)) should be(None)
        }
      }
    }
    val item = item3
    val review = review3
    test("%s should not return score for %s".format(actor, item)) {
//      scorer.score(actor, item) should be(None)
    }
    test("%s should not return score for %s without %s".format(actor, item, review)) {
//      scorer.score(actor, item, Set(review)) should be (None)
    }
  }

}
