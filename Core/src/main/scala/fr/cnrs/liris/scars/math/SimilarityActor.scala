package fr.cnrs.liris.scars.math

import fr.cnrs.liris.scars.api._
/**
 * @author  Simon Meyffret
 * @version 0.0.1
 * @since   scala 2.8
 * Date: 3 fev. 2011
 * Time: 13:06:02
 */

object SimilarityActor {
  
  def sequential(database: Database, unique: Boolean = true, positive: Boolean = true) = {
    val similarity = new SimilarityActor(positive, unique)
    database.actors.flatMap { actor =>
      similarity.computeSimilarity(actor)
    }
  }

  def parallele(database: Database, unique: Boolean = true, positive: Boolean = true) = {
    database.actors.par.flatMap { actor =>
      new SimilarityActor(positive, unique).computeSimilarity(actor)
    }
  }
  
}

class SimilarityActor(positive: Boolean = false, uniqueId: Boolean = false, onlyFriends: Boolean = false) extends SimilarityImpl[Actor,Item](positive, uniqueId) {
  
  def friends(subject: Actor)(in: Set[Actor]) = subject.friends & in
  
  override def filters(subject: Actor) = {
    super.filters(subject) compose (if(onlyFriends) friends(subject) else identity)
  }
  
  def commonRaters(actor: Actor, without: Set[Review] = Set.empty) = {
    actor.reviews.              // all reviews of this subject
      diff(without).            // without provided reviews
      flatMap(_.item.reviews).  // we take all reviews regarding items
      diff(without).            // without provided reviews again
      map(_.actor) - actor      // we return corresponding subjects, without the requested one
  }

  def ratingsMap(actor: Actor, without: Set[Review]): Map[Item, Double] = {
    actor.reviews.diff(without).view.map(
      review => (review.item -> review.rating)
    ).toMap
  }

}