package fr.cnrs.liris.scars.scorer.social

import fr.cnrs.liris.scars.api._
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

abstract class ActorSocialScorer(hops: Int, depth: Int, alpha: Double = 0.5) extends SocialScorer(hops, depth, alpha) with ActorScorer {

  require(depth >= 0, "depth=%d must be positive".format(depth))
  require(hops > 0, "hops=%d must be strictly positive".format(hops))

  def scores(actor: Actor, reviews: Set[Review], without: Set[Review]) = {
    social_scores(actor, reviews, defaultParent, without, hops, depth)
  }

  def social_scores(actor: Actor, reviews: Set[Review], parent: Parent, without: Set[Review], hops: Int, n: Int): Map[Review, Option[Score]]

  def scores_0(actor: Actor, reviews: Set[Review], without: Set[Review]) = {
    for {
      review <- reviews
      score <- score_0(actor, review.item, add_without(reviews, without, review))
    } yield (review -> Some(score))
  }.toMap
  
  def friends(actor: Actor, parent: Parent, reviews: Set[Review], without: Set[Review]): Set[Actor] = {
    friends(actor, parent)
  }

//  def weight(actor: Actor, friend: Actor, item: Item, without: Set[Review]) = actor.trust(friend)
//
//  def weight_ratings(scores: Iterable[(Score, Double)], n: Int) = {
//    scores.map {
//      case (rating, weight) =>
//        (rating.value * weight, weight)
//    }
//  }
//
//  def friends_score(friendsScores: Iterable[Option[(Score, Double)]], n: Int) = {
//    val flattenedScores = friendsScores.flatten
//    val (scores, weights) = weight_ratings(flattenedScores, n).unzip
//    for {
//      score <- aggregate_scores(scores, weights)
//      count = flattenedScores.map(_._1.count).sum + 1
//    } yield Score(score, count = count)
//  }
//  
//  def aggregate_scores(scores: Iterable[Double], weights: Iterable[Double]) = {
//    val scoreSum = scores.sum
//    val weightSum = weights.sum
//
//    if (weightSum == 0)
//      None
//    else {
//      Some(scoreSum / weightSum)
//    }
//  }
//
//  def merge_scores(actor: Option[Score], friends: Option[Score]) = {
//    val scores = 
//      actor.map { _ -> alpha } ::
//      friends.map { _ -> (1 - alpha) } ::
//      Nil
//    for {
//      score <- friends_score(scores, depth)
//    } yield score.copy(count = score.count - 1)
//  }

  def add_without(reviews: Set[Review], without: Set[Review], review: Review) = without ++ (reviews & Set(review))

  override def toString = "msocial_h%d_n%d".format(hops, depth)

}
