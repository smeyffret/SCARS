package fr.cnrs.liris.scars.api

import fr.cnrs.liris.scars.api.impl._

/**
 * @author  Simon Meyffret
 * @version 0.0.1
 * @since   scala 2.8
 * Date: 31 janv. 2011
 * Time: 16:55:32
 */

class MockActor(id: Int) extends DefaultActor(id) {
  override def meanRating = computeMeanRating
  override def meanRating(without: Set[Review]) = computeMeanRating
  override def toString = "Actor(%d)".format(id)
}

class MockItem(id: Int) extends DefaultItem(id, id.toString) {
  override def toString = "Item(%d)".format(id)
  category = MockCategory
}

class MockReview(actor: Actor, item: Item, rating: Double) 
	extends DefaultReview(actor, item, rating) {
  override def toString = "Review(%s)".format(super.toString)
}

case object MockCategory extends DefaultCategory(0, "mock")
