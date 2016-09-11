package fr.cnrs.liris.scars.api

import scala.collection.mutable.Map
import fr.cnrs.liris.scars.api.Conversion._

class MockDatabase(val isLeaveOneOut: Boolean = false) extends Database {
  
  val ratingRange = Nil
  
  var actorsMap = Map[Int, MockActor]()
  var itemsMap = Map[Int, MockItem]()
  val reviewsMap = Map[(Actor, Item), Review]() 
  
  def actors = actorsMap.values
  def items = itemsMap.values
  def reviews = reviewsMap.values
  val evalReviews = Set[Review]()

  def update(actor: MockActor, item: MockItem, rating: Double): Review = {
    val review = new MockReview(actor, item, rating)
    actor add review
    item add review
    actorsMap += (actor.id -> actor)
    itemsMap += (item.id -> item)
    reviewsMap += ((actor, item) -> review)
    review
  }
  
  def trust(actor: MockActor, friend: MockActor, trust: Double) {
    actor addTrust (friend, trust)
    friend addFollower actor
  }
  
  def similar(actor: MockActor, other: MockActor, correlation: Double) {
    actor addSimilarity (other, correlation)
  }
  
  def getActor(id: Int) = actorsMap.getOrElseUpdate(id, toActor(id))
  def getItem(id: Int) = itemsMap.getOrElseUpdate(id, toItem(id))

}
