package fr.cnrs.liris.scars.scorer.soa

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
class TrustWalkerSimpleFriendsTest extends FunSuite with ShouldMatchers {
  
  implicit val database = new MockDatabase()
  import database.trust
  trust(1, 2, 1)
  trust(2, 3, 1)

  database(1, 1) = 0.1
  database(3, 1) = 0.3

  test("TrustWalker0 should not reach a friend of friend") {
    implicit val scorer = new TrustWalker(0)
    score(1, 1) should be (None)
  }
  
  test("TrustWalker1 should not reach a friend of friend") {
    implicit val scorer = new TrustWalker(1)
    score(1, 1) should be (None)
  }
  
  test("TrustWalker2 should reach a friend of friend") {
    implicit val scorer = new TrustWalker(2)
    score(1, 1) should be (Some(0.3))
  }
  
  test("TrustWalker3 should stop at the friend of friend with rating") {
    implicit val scorer = new TrustWalker(3)
    score(1, 1) should be (Some(0.3))
  }
  
  def score(actor: Actor, item: Item)(implicit scorer: Scorer) = {
    val without = actor.review(item).toSet
    scorer.score(actor, item, without).map(_.value)
  }

}
