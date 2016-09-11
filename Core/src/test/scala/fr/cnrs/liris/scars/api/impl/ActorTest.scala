package fr.cnrs.liris.scars.api.impl

import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import fr.cnrs.liris.scars.api._

/**
 * @author  Simon Meyffret
 * @version 0.0.1
 * @since   scala 2.8
 * Date: 3 fev. 2011
 * Time: 13:06:02
 */

@RunWith(classOf[JUnitRunner])
class ActorTest extends FunSuite with ShouldMatchers {

  val actor = new DefaultActor(1)
  val items = (1 to 4)
  val reviews: Set[Review] = items.map { itemId =>
    val rating = itemId
    val item = new DefaultItem(itemId, itemId.toString)
    val review = new DefaultReview(actor, item, rating)
    item add review
    actor add review
    review
  }.toSet
  
  test("Test the actor's mean with all reviews") {
    val mean = (1 + 2 + 3 + 4) / 4.
    actor.meanRating should be(Some(mean))
    actor.meanRating(Set()) should be(Some(mean))
  }
  
  test("Test the actor's mean without one reviews") {
    val without = reviews.filter{ r => r.item.id == 3 }
    actor.meanRating(without) should be(Some((1 + 2 + 4) / 3.))
  }
  
  test("Test the actor's mean without half reviews") {
    val without = reviews.filter{ r => r.item.id % 2 != 0 }
    actor.meanRating(without) should be(Some((2 + 4) / 2.))
  }
  
  test("Test the actor's mean without all but one review") {
    val without = reviews.filterNot{ r => r.item.id == 3 }
    actor.meanRating(without) should be(Some(3))
  }
  
  test("Test the actor's mean without all reviews") {
    actor.meanRating(reviews) should be(None)
  }
  
}
