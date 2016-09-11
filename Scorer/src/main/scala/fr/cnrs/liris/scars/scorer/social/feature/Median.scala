package fr.cnrs.liris.scars.scorer.social.feature

import fr.cnrs.liris.scars.api._
import fr.cnrs.liris.scars.scorer.social._

/**
 * @author  Simon Meyffret
 * @version 0.0.1
 * @since   scala 2.8
 * Date: 21 févr. 2011
 * Time: 14:18:18
 */


trait Median extends SocialScorer {
  
  private val epsilon = 0.000001

  //TODO: gérer le "count"
  abstract override def friends_score(friendsScores: Iterable[Option[(Score, Double)]]) = {
    if (friendsScores.isEmpty)
      None
    else
      findMedian(friendsScores.flatten.toSeq.sortBy(_._1.value))
  }
  
  /// the collection must be sorted by rating value and not be empty
  def findMedian(scores: Seq[(Score, Double)]) = {
    val (ratings, weights) = scores.unzip
	val (summedWeight, medium) = sum(weights)
    ratings.zip(summedWeight).find{ case (rating, weight) =>
      weight + epsilon >= medium
    }.map(_._1)
  }
  
  def sum(weights: Seq[Double]) = {
    //TODO: vérifier que c'est toujours correct'
    var sum = 0.0
    val summed = weights.map{ weight =>
      sum += weight
      sum
    }
    (summed, sum / 2)
  }

  override def toString = "median_" + super.toString

}
