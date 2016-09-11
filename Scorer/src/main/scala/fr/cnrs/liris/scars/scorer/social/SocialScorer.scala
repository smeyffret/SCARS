package fr.cnrs.liris.scars.scorer.social

import fr.cnrs.liris.scars.api._
import fr.cnrs.liris.scars.scorer.algo.Aggregation
import parent._

/**
 * A scorer which estimates the score of an item by a actor using the actor's friends' scores.
 * To estimate an item scored by a actor, it ask the actor friends score for this item and
 * combine them. For each friend, if it didn't scored the item, then it ask to his friends...
 *
 * @author  Simon Meyffret
 * @version 0.0.1
 * @since   scala 2.8
 * Date: 18 sept. 2010
 * Time: 18:54:25
 *
 * @param similarity relations between actor to get friends and friends coefficients
 * @param scores scores already defined in the system, used to get friends scores
 * @param hops maximum scores you can ask in the same relation branch
 * @param depth maximum depth you can go in a branch to ask for score
 * @param alpha0 the weight of actor's score regarding his friends
 * @param beta0 the weight of friends' score regarding a actor
 */

abstract class SocialScorer(val hops: Int, val depth: Int, val alpha: Double = 0.5) extends Scorer with Aggregation with Parent {

  require(depth >= 0, "depth=%d must be positive".format(depth))
  require(hops > 0, "hops=%d must be strictly positive".format(hops))

  def score(actor: Actor, item: Item, without: Set[Review]) = {
    social_score(actor, item, defaultParent, without, hops, depth)
  }

  def social_score(actor: Actor, item: Item, parent: Parent, without: Set[Review], hops: Int, n: Int): Option[Score]

  def score_0(actor: Actor, item: Item, without: Set[Review]) = {
    rating(actor, item, without)
  }
  
  def friends(actor: Actor, parent: Parent, item: Item, without: Set[Review]): Set[Actor] = {
    friends(actor, parent)
  }

  def weight(actor: Actor, friend: Actor, item: Item, without: Set[Review], score: Score): Option[Double]

  def friends_score(friendsScores: Iterable[Option[(Score, Double)]]) = {
    aggregate(friendsScores)
  }
  
  def merge_scores(actor: Option[Score], friends: Option[Score]) = {
    val scores = 
      actor.map { _ -> alpha } ::
      friends.map { _ -> (1 - alpha) } ::
      Nil
    for {
      score <- friends_score(scores)
    } yield score
  }
  
  override def toString = "social_h%d_n%d".format(hops, depth)

}
