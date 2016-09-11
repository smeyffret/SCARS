package fr.cnrs.liris.scars.scorer.social.feature

import fr.cnrs.liris.scars.api._
import fr.cnrs.liris.scars.scorer.social._
import fr.cnrs.liris.scars.scorer.algo.Aggregation
import fr.cnrs.liris.scars.scorer.confidence.{Confidence => ConfidenceFactory}

/**
 * @author  Simon Meyffret
 * @version 0.0.1
 * @since   scala 2.8
 * Date: 21 févr. 2011
 * Time: 14:18:18
 */

object Confidence {
  trait Factory {
    def rating() = ConfidenceFactory.rating()
    def score() = ConfidenceFactory.score()
  }

  trait DefaultScore extends Factory {
    override def score() = ConfidenceFactory.defaultScore()
  }

  trait TrustWalker extends Factory {
    override def rating() = ConfidenceFactory.none()
    override def score() = ConfidenceFactory.walker()
  }
}

trait Confidence extends Scorer with Aggregation with Confidence.Factory {
  
  val ratingConfidence = rating()
  val scoreConfidence = score()
  
  abstract override def aggregate(friendsScores: Iterable[Option[(Score, Double)]]) = {
    for {
      rating <- super.aggregate(friendsScores)
      confidence = scoreConfidence.confidenceSome(friendsScores)
    } yield {
      rating.copy(confidence = confidence)
    }
  }
  
  override def rating(review: Review) = {
    val confidence = ratingConfidence.confidence(review)
    super.rating(review).copy(confidence = confidence)
  }
  
  override def toString = "confident_" + super.toString

}

trait SocialConfidence extends SocialScorer with Confidence {
  override def weight(actor: Actor, friend: Actor, item: Item, without: Set[Review], score: Score): Option[Double] = {
    Some(score.confidence)
  }
}