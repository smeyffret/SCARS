package fr.cnrs.liris.scars.math

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
class SimilarityItemActorMeanTest extends FunSuite with ShouldMatchers {
  
  implicit val database = new MockDatabase()
  import database.trust
  trust(1, 2, 1)
  trust(2, 3, 1)

  database(1, 1) = 1.1
  database(1, 2) = 1.2
  database(1, 3) = 1.3
  
  database(2, 2) = 2.2
  database(2, 3) = 2.3
  database(2, 42) = 2.1
  
  database(3, 1) = 3.1
  database(3, 2) = 3.2
  database(3, 42) = 3.3
  
  // 1 -> 1,    3
  // 2 -> 1, 2, 3
  // 3 -> 1, 2

  {
    val without = Set.empty[Review]
    has_sim(1, 2, without)
    has_sim(2, 3, without)
    has_no_sim(1, 3, without)
  }

  
  def score(actor: Actor, item: Item)(implicit scorer: Scorer) = {
    val without = actor.review(item).toSet
    scorer.score(actor, item, without)
  }
  
  def has_sim(left: Item, right: Item, without: Set[Review] = Set.empty) {
    test("Items %s and %s should share positive similarity without %s".format(left, right, without)) {
      val sim = new SimilarityItemActorMean(positive = true).similarity(left, right, without)
      sim should be ('defined)
      sim.get should be > (0.)
    }
  }

  def has_no_sim(left: Item, right: Item, without: Set[Review] = Set.empty) {
    test("Items %s and %s should share no similarity without %s".format(left, right, without)) {
      val sim = new SimilarityItemActorMean(positive = true).similarity(left, right, without)
      sim should be ('empty)
    }
  }

}
