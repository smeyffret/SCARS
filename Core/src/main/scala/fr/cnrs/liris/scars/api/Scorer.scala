package fr.cnrs.liris.scars.api

/**
 * A scorer estimates the score of an item by an actor
 *
 * @author  Simon Meyffret
 * @version 0.0.1
 * @since   scala 2.8
 * Date: 29 nov. 2010
 * Time: 15:57:04
 */


case class Score(val value: Double, confidence: Double = 1.0, count: Int = 1, actors: Int = 1, time: Double = 0.)
case class Undefined(count: Int = 1, actors: Int = 1, time: Double = 0.)

trait Scorer {
/**
 * Request a score for an item from an actor, without using reviews from the set 'without'
 *
 * @param actor the actor who may score the item
 * @param item the item concerned
 * @param without existing reviews which may not be used to compute the score (default empty set)
 * @return Some(score) if estimated, None otherwise
 */
  def scores(review: Review, without: Set[Review] = Set.empty) = {
    Map(review -> score(review.actor, review.item, without + review))
  }
  def score(actor: Actor, item: Item): Option[Score] = score(actor, item, Set.empty)
  def score(actor: Actor, item: Item, without: Set[Review]): Option[Score]
  
  def rating(actor: Actor, item: Item, without: Set[Review]): Option[Score] = {
    for {
      review <- actor.review(item) if !(without.contains(review))
    } yield (rating(review))
  }
  
  def rating(review: Review) = Score(review.rating)
  
}

trait ActorScorer extends Scorer {
  
/**
 * Request scores for all items rated by the actor, without using the corresponding reviews
 *
 * @param actor the actor who have made the reviews
 * @param reviews the concerned reviews
 * @param without existing reviews which may not be used to compute the score (default empty set)
 * @return Map[Review, Option(score)] one optional score foreach review
 */
  def scores(actor: Actor, reviews: Set[Review], without: Set[Review]): Map[Review, Option[Score]]
  
}

trait ItemScorer extends Scorer {
  
/**
 * Request scores for an item from all actors, without using the corresponding reviews
 *
 * @param item the item concerned
 * @param reviews the concerned reviews
 * @param without existing reviews which may not be used to compute the score (default empty set)
 * @return Map[Review, Option(score)] one optional score foreach review
 */
  def scores(item: Item, reviews: Set[Review], without: Set[Review]): Map[Review, Option[Score]]
  
}
