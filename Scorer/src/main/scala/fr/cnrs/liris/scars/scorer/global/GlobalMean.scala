package fr.cnrs.liris.scars.scorer.global

import fr.cnrs.liris.scars.api._
import fr.cnrs.liris.scars.scorer.algo.Aggregation

/**
 * A scorer which estimates the score by a actor for any item,
 * by giving the mean score of this actor (independent of the item) 
 *
 * @author  Simon Meyffret
 * @version 0.0.1
 * @since   scala 2.8
 * Date: 29 nov. 2010
 * Time: 16:42:27
 *
 * @param score_0 the data set containing scores, to computed the mean
 */

class GlobalMean extends Scorer with Aggregation {

  def score(actor: Actor, item: Item, without: Set[Review]) = {
    val scores = (item.reviews &~ without).toStream.map { review =>
      for{
        rating <- rating(review.actor, item, without)
      } yield rating -> 1.0
    }
    aggregate(scores)
  }

  override def toString = "GlobalMean"

}
