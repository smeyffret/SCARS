package fr.cnrs.liris.scars.math

import fr.cnrs.liris.scars.api._
/**
 * @author  Simon Meyffret
 * @version 0.0.1
 * @since   scala 2.8
 * Date: 3 fev. 2011
 * Time: 13:06:02
 */
class SimilarityItemActorMean(positive: Boolean = false, uniqueId: Boolean = false) extends SimilarityItem(positive, uniqueId) {

  override def raw_similarity(item: Item, other: Item,
                              without: Set[Review] = Set.empty) = {
    val commons = commonRaters(item, other)
    var numerator, denumX, denumY = 0. // all initialized
    for {
      user <- commons
      userMean <- user.meanRating(without)
      leftReview <- user.review(item)
      rightReview <- user.review(other)
    } {
      val leftRating = leftReview.rating - userMean
      val rightRating = rightReview.rating - userMean
      numerator += leftRating * rightRating
      denumX += leftRating * leftRating
      denumY += rightRating * rightRating
    }
    
    if (denumX != 0 && denumY != 0) {
      Some(numerator / math.sqrt(denumX * denumY))
    } else {
      None
    }
  }
  
  private def commonRaters(item: Item, other: Item) = {
    val userLeft = item.reviews.map(_.actor)
    val userRight = other.reviews.map(_.actor)
    userLeft & userRight
  }

}
