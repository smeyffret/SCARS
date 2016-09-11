package fr.cnrs.liris.scars.scorer.soa

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import fr.cnrs.liris.scars.api._
import fr.cnrs.liris.scars.math._
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
class TrustWalkerSimpleItemsTest extends FunSuite with ShouldMatchers {
  
  implicit val database = new MockDatabase()
  import database.trust
  trust(1, 2, 1)
  trust(2, 3, 1)

  database(1, 1) = 1.1
  database(1, 2) = 1.2
  database(1, 3) = 1.3
  database(1, 4) = 1.4
  
  database(2, 2) = 2.2
  database(2, 3) = 2.3
  
  database(3, 1) = 3.1
  database(3, 2) = 3.2
  database(3, 4) = 3.4
  
////  database(42, 1) = 42.1
//  database(42, 2) = 42.2
////  database(42, 3) = 42.3
//  database(42, 4) = 42.4
////  database(43, 1) = 43.1
//  database(43, 2) = 43.2
////  database(43, 3) = 43.3
//  database(43, 4) = 43.4
//  database(44, 2) = 404.2
//  database(44, 4) = 44.4
  
  //                sim to item(actors)
  // 1 -> 1,    3   sim to 2 (1,3) and 4 (1,3)
  // 2 -> 1, 2, 3   sim to 1 (1,3) and 3 (1,2) and 4 (1,3)
  // 3 -> 1, 2      sim to 2 (1,2)
  // 4 -> 1,    3

  test("TrustWalker1 should return a similar item since it can't reach friend of friend") {
    implicit val scorer1 = new TrustWalker(1)// { override val MaxTry = 1 }// { override def log(x: Any*) = println(x) }
    (1 to 100).foreach{ _ =>
      score(1, 1) should (be (Some(1.2)) or be (Some(2.2)))
    }
  }
  
  test("TrustWalker2 should return a similar item or a friend of friend score") {
    implicit val scorer2 = new TrustWalker(2)// { override val MaxTry = 1 }// { override def log(x: Any*) = println(x) }
    val results = (1 to 100).map{ it =>
      val result = score(1, 1)
      result should (be (Some(1.2)) or be (Some(2.2)) or be (Some(3.1)))
      result
    }.groupBy(identity).map{ case (x, l) => x -> l.size }
//    println(results)
  }
  
  def score(actor: Actor, item: Item)(implicit scorer: TrustWalker) = {
    val without = actor.review(item).toSet
    val sims = scorer.compute_similarity(item, without)
    scorer.random_walk(actor, item, without, 0, sims).map(_.value)
  }
  
}
