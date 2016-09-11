package fr.cnrs.liris.scars.math

import fr.cnrs.liris.scars.api._


class ExtendedSimilarityActor(scorer: Scorer, positive: Boolean = false, uniqueId: Boolean = false, onlyFriends: Boolean = false) extends SimilarityActor(positive, uniqueId, onlyFriends) {

  override def commonRaters(actor: Actor, without: Set[Review] = Set.empty) = {
    if (onlyFriends) {
      actor.friends
    } else {
      super.commonRaters(actor, without)
    }
  }
  
  override def mergeRatings(subject: Actor, subjectRatings: Map[Item, Double])
                  (other: Actor, otherRatings: Map[Item, Double]) = {
    val subjectMissing = complement(subject)(otherRatings.keySet &~ subjectRatings.keySet)
    val otherMissing = complement(other)(subjectRatings.keySet &~ otherRatings.keySet)
    util.unionMaps(subjectRatings ++ subjectMissing, otherRatings ++ otherMissing)
  }
  
  def complement(actor: Actor)(missing: Set[Item]) = {
    for {
      item <- missing
      score <- scorer.score(actor, item)
    } yield (item -> score.value)
  }.toMap

}