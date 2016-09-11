package fr.cnrs.liris.scars.scorer.social.impl

import fr.cnrs.liris.scars.api._
import fr.cnrs.liris.scars.scorer.social._

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

abstract class RecSocialScorer(hops: Int, depth: Int, alpha: Double = 0.5) extends SocialScorer(hops, depth, alpha) {

  def social_score(actor: Actor, item: Item, parent: Parent, without: Set[Review], hops: Int, n: Int) = n match {
    case neg if (neg < 0) =>
      throw new IllegalArgumentException("depth=%d must be positive".format(n))
    case 0 =>
      score_0(actor, item, without)
    case _ =>
      score_n(actor, item, parent, without, hops, n)
  }

  def score_n(actor: Actor, item: Item, parent: Parent, without: Set[Review], hops: Int, n: Int): Option[Score] = {
    val actorScore = score_0(actor, item, without)

    val newHops = if (actorScore.isDefined) hops - 1 else hops
    val friendsScore = friends_score_n(actor, item, parent, without, newHops, n)

    merge_scores(actorScore, friendsScore)
  }

  def friends_score_n(actor: Actor, item: Item, parent: Parent, without: Set[Review], hops: Int, n: Int): Option[Score] = hops match {
    case neg if (neg < 0) =>
      throw new IllegalArgumentException("hops=%d must be positive".format(hops))
    case 0 =>
      None
    case _ =>
      val friendsScores = friends(actor, parent, item, without).toStream.map { friend => // lazy pour ne pas perdre les doublons (attention, view entraine de recalculer le score à chaque accès!)
        for {
        score <- social_score(friend, item, parents(parent, actor), without, hops, n - 1)
        weight <- weight(actor, friend, item, without, score) if (weight != 0)
        } yield (score, weight)
      }

      friends_score(friendsScores)
  }
  
  override def toString = "rec_" + super.toString

}
