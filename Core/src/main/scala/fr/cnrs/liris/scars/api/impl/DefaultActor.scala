package fr.cnrs.liris.scars.api.impl

import fr.cnrs.liris.scars.api._
import fr.cnrs.liris.scars.math.Math

class DefaultActor(val id: Int) extends Actor {
  
  var friendsCount = 0
  var reviewsCount = 0

  def add(review: Review) = {
    reviewsCount += 1
    _reviews = null
    items += (review.item -> review)
  }

  def addExpertise(category: DefaultCategory, expertise: Expertise.Value) = {
    expertCategories += (category -> expertise)
    category.addExpert(this)
  }

  def addTrust(friend: Actor, trust: Double) = {
    friendsCount += 1
    trusts += (friend -> trust)
    friends += friend
  }

  def addFollower(friend: Actor) = {
    followers += friend
  }

  def addSimilarity(other: Actor, correlation: Double) = {
    similarities += (other -> correlation)
  }

  private var similarities = Map[Actor, Double]()
  private var trusts = Map[Actor, Double]()
  protected var items = Map[Item, Review]()
  private var expertCategories = Map[Category, Expertise.Value]()

  var rank: Option[Int] = None
  def similars = similarities.keySet
  var friends = Set[Actor]()
  var followers = Set[Actor]()
  private var _reviews: Set[Review] = null
  def reviews = {
    if (_reviews == null)
      _reviews = items.values.toSet
    _reviews
  }
  def expertises = expertCategories.keySet

  private val util = new Math()
  // Attention : un map sur un Set renvoi un set, sans le 'view', les doublons dans les ratings sont supprimés...
  def meanRating = _meanRating
  private lazy val _meanRating = computeMeanRating
  protected def computeMeanRating = util.meanBy(reviews)(_.rating)

  def meanRating(without: Set[Review]) = {
    val inter = reviews & without
    if (inter.isEmpty) {
      meanRating
    } else {
      val size = reviews.size
      val interSize = inter.size
      val newSize = size - interSize
      if (newSize == 0) { // no more available reviews
        None
//      } else if (newSize < interSize) { // quicker to compute a new mean
//        (reviews &~ without).view.map(_.rating).reduceLeftOption(_+_).map(_ / newSize)
      } else { // quicker to adapte past mean
        meanRating.map { mean =>
          (mean * size - inter.view.map(_.rating).sum) / (newSize)
        }
      }
    }
  }

  def trust(other: Actor) = trusts get other
  def similarity(other: Actor) = similarities get other
  def review(item: Item) = items get item
  def expertise(category: Category) = expertCategories get category

}