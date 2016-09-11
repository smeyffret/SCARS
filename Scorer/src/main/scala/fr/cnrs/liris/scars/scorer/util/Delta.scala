package fr.cnrs.liris.scars.scorer.util

import fr.cnrs.liris.scars.api._
import fr.cnrs.liris.scars.math.Math

/**
 * This scorer is similar to SocialScorer. Except that it computed delta score.
 * A delta score is the difference between an item score and the mean of the actor score.
 * Each friend return only the delta score. Then the actor add this delta to his mean to
 * compute the absolute score.
 * @see SocialScorer
 *
 * @author  Simon Meyffret
 * @version 0.0.1
 * @since   scala 2.8
 * Date: 29 nov. 2010
 * Time: 22:42:52
 */
case class DeltaSettings(min: Double, max: Double, defaultMean: Option[Double])

trait Delta extends Scorer {

  private val util = new Math()
  val settings: DeltaSettings
  val minReviewsSize = 1
//  require(minReviewsSize > 0, "minReviewsSize=%d must be strictly positive".format(minReviewsSize))

  //TODO: utilisation de without pour calculer la moyenne ?
  abstract override def score(actor: Actor, item: Item, without: Set[Review]) = {
    for {
      score <- super.score(actor, item, without)
      mean <- meanRating(actor, without)
      deltaScore = score.value + mean
    } yield {
      Score(borne(deltaScore)) // ensure min <= deltaScore <= max
    }
  }

  abstract override def rating(actor: Actor, item: Item, without: Set[Review]) = {
    for {
      rating <- super.rating(actor, item, without)
      meanRating <- meanRating(actor, without)
    } yield Score(rating.value - meanRating)
  }

  private def meanRating(actor: Actor, without: Set[Review]) = {
    val reviews = actor.reviews &~ without
    val size = reviews.size
    if (size >= minReviewsSize) {
      util.meanBy(reviews)(_.rating)
    } else {
      settings.defaultMean
    }
  }

  private def borne(score: Double) = {
    import settings._
    score.min(max).max(min)
  }

  override def toString = "Rel" + super.toString

}
