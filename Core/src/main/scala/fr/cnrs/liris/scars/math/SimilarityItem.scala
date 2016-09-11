package fr.cnrs.liris.scars.math

import fr.cnrs.liris.scars.api._
/**
 * @author  Simon Meyffret
 * @version 0.0.1
 * @since   scala 2.8
 * Date: 3 fev. 2011
 * Time: 13:06:02
 */

object SimilarityItem {
  
  def sequential(database: Database, unique: Boolean = true, positive: Boolean = true) = {
    val similarity = new SimilarityItem(positive, unique)
    database.items.flatMap { item =>
      similarity.computeSimilarity(item)
    }
  }

  def parallele(database: Database, unique: Boolean = true, positive: Boolean = true) = {
    database.items.par.flatMap { item =>
      new SimilarityItem(positive, unique).computeSimilarity(item)
    }
  }
  
}

class SimilarityItem(positive: Boolean = false, uniqueId: Boolean = false) extends SimilarityImpl[Item,Actor](positive, uniqueId) {
  
  def commonRaters(item: Item, without: Set[Review] = Set.empty) = {
    item.reviews.               // all reviews of this item
      diff(without).            // without provided reviews
      flatMap(_.actor.reviews). // we take all reviews regarding items
      diff(without).            // without provided reviews again
      map(_.item) - item        // we return corresponding subjects, without the requested one
  }

  def ratingsMap(item: Item, without: Set[Review]): Map[Actor, Double] = {
    item.reviews.diff(without).view.map(
      review => (review.actor -> review.rating)
    ).toMap
  }
  
  def rating(review: Review) = review.rating

}