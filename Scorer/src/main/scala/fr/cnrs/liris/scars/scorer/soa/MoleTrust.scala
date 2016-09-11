package fr.cnrs.liris.scars.scorer.soa

import fr.cnrs.liris.scars.api._
import fr.cnrs.liris.scars.math.Math
import fr.cnrs.liris.scars.scorer.algo.Aggregation

class MoleTrust(depth: Int) extends ActorScorer with Aggregation {
  
  private val util = new Math()
  
  type TrustNetwork = Map[Actor, (Int, Double)]
  
  def scores(actor: Actor, reviews: Set[Review], without: Set[Review]): Map[Review, Option[Score]] = {
    val friends = getFriends(actor)
    for {
      review <- reviews
    } yield (
      review -> score(actor, review.item, without + review, friends)
    )
  }.toMap

  def score(actor: Actor, item: Item, without: Set[Review]) = {
    val friends = getFriends(actor)
    score(actor, item, without, friends)
  }
  
  def score(actor: Actor, item: Item, without: Set[Review], friends: TrustNetwork) = {
    val scores = friends.toStream.map{ case (friend, (_, trust)) =>
      for {
        rating <- rating(friend, item, without)
      } yield (rating, trust)
    }
    aggregate(scores)
  }
  
  def getFriends(actor: Actor) = {
    buildFriends(actor, 1)
  }
  
  protected def buildFriends(actor: Actor, n: Int): TrustNetwork = {
    val friends = immediateFriends(actor, n).toMap
    if (n == depth) {
      friends
    } else {
      friends ++ friends.keys.flatMap(buildFriends(_, n + 1))
    }
  }
  
  protected def immediateFriends(actor: Actor, n: Int) = {
    for {
      friend <- actor.friends
      trust <- actor.trust(friend)
    } yield (friend -> (n, trust))
  }
  
  override def toString = "MoleTrust" + depth
  
}
