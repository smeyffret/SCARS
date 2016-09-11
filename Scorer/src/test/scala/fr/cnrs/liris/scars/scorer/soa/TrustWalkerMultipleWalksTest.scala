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
class TrustWalkerMultipleWalksTest extends FunSuite with ShouldMatchers {
  
  implicit val database = new MockDatabase()
  import database.trust
  trust(1, 2, 1)
  trust(2, 3, 1)
  trust(2, 4, 1)

  database(1, 1) = 1.1
  database(1, 2) = 1.2
  database(1, 3) = 1.3
  database(1, 4) = 1.4
  database(1, 5) = 1.5
  database(1, 6) = 1.6
  
  database(2, 2) = 2.2
  
  database(3, 1) = 3.1
  database(3, 2) = 3.2
  database(3, 4) = 3.4
  database(3, 5) = 3.5
  
  database(4, 1) = 4.1
  database(4, 2) = 4.2
  database(4, 4) = 4.4
  database(4, 5) = 4.5



  database(5, 1) = 5.1
  database(5, 2) = 5.2
  database(5, 3) = 5.3
  database(5, 4) = 5.4
  database(5, 5) = 5.5
  database(5, 6) = 5.6
  
  database(6, 1) = 6.1
  database(6, 2) = 6.2
  database(6, 3) = 6.3
  database(6, 4) = 6.4
  database(6, 5) = 6.5
  database(6, 6) = 6.6
  
  database(7, 1) = 7.1
  database(7, 2) = 7.2
  database(7, 3) = 7.3
  database(7, 4) = 7.4
  database(7, 5) = 7.5
  database(7, 6) = 7.6
  
  //                   sim to item(actors)
  // 1 -> 1,    3      sim to 2 (1,3) and 4 (1,3)
  // 2 -> 1, 2, 3      sim to 1 (1,3) and 3 (1,2) and 4 (1,3)
  // 3 -> 1, 2         sim to 2 (1,2)
  // 4 -> 1,    3
  // 5 -> 1,    3
  
  def buildTrustWalker(k: Int) = new TrustWalker(k) {
//    override val MaxTry = 10
    override def proba_picking_similar_item(sims: Similarities, k: Int) = 0
  }
  
  {
    implicit val scorer1 = buildTrustWalker(1)
      score(1, 1, Some(2.2)) // last step, take a similar item (2,3)
      score(1, 2, Some(2.2)) // last step, take a similar item (1,3)
      score(1, 3, Some(2.2)) // last step, take a similar item (1,2)
      score(1, 4, None) // no similar item
      score(1, 5, None) // no similar item
      score(1, 6, None) // no similar item
  }
  
  {
    implicit val scorer2 = buildTrustWalker(2)
      score(1, 1, 3.1, 4.1) // 4's score or 3's score averaged
      score(1, 2, Some(2.2))// 2's score
      score(1, 4, 3.4, 4.4) // 4's score or 3's score averaged
      score(1, 5, 3.5, 4.5) // 4's score or 3's score averaged

      score(1, 3, 3.1, 4.2) // last step, take a similar item (1,2) in 3 and 4's scores
      score(1, 6, 3.4, 4.5) // last step, take a similar item (4,5) in 3 and 4's scores
  }
  
  val NB_TRY = 100
  
  def score(actor: Actor, item: Item, result: Option[Double])(implicit scorer: Scorer) {
    test("%s should return %s for %s and %s".format(scorer, result, actor, item)) {
      (1 to NB_TRY).foreach { _ =>
        score(actor, item)(scorer).map(_.value) should be (result)
      }
    }
  }
  
  def score(actor: Actor, item: Item, from: Double, to: Double)(implicit scorer: Scorer) {
    test("%s should return between %f and %f for %s and %s".format(scorer, from, to, actor, item)) {
      (1 to NB_TRY).foreach { _ =>
        val r = score(actor, item)(scorer)
        r should be ('defined)
        r.get.value should be >=(from)
        r.get.value should be <=(to)
      }
    }
  }
  
  def score(actor: Actor, item: Item)(implicit scorer: Scorer) = {
    val without = actor.review(item).toSet
    scorer.score(actor, item, without)
  }
  
}
