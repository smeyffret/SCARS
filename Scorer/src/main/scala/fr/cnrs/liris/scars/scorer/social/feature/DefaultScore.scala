package fr.cnrs.liris.scars.scorer.social.feature

import fr.cnrs.liris.scars.api._
import fr.cnrs.liris.scars.math.Math
import fr.cnrs.liris.scars.scorer.confidence.Variance
import fr.cnrs.liris.scars.scorer.social._
import fr.cnrs.liris.scars.scorer.global.ItemBased


trait DefaultScore extends SocialScorer {
  
  protected val rand = new scala.util.Random(System.currentTimeMillis)
  protected val ProbaRating = 0.02  // best value found by experiments
  protected val alphaDefaultConfidence = 0.5

  abstract override def social_score(actor: Actor, item: Item, parent: Parent, without: Set[Review], hops: Int, n: Int) = {
    val score = super.social_score(actor, item, parent, without, hops, n)
    if (score.isEmpty && return_rating)
      default_score(actor, item, without)
    else
      score
  }
  
  def default_score(actor: Actor, item: Item, without: Set[Review]): Option[Score] = None
  
  private def return_rating = rand.nextDouble < ProbaRating
  
  override def toString = "%.2f_".format(ProbaRating) + super.toString
}

trait DefaultMean extends DefaultScore with Variance {
  
  private val util = new Math()

  override abstract def default_score(actor: Actor, item: Item, without: Set[Review]) = {
    super.default_score(actor, item, without)
  }
  
  def default_reviews_score(reviews: Set[Review]) = {
    val ratings = reviews.map(_.rating)
    for {
      score <- util.mean(ratings)
      count = ratings.size
      confidence = variance_confidence(ratings) /* util.sigmoid(count - 1)*/ * alphaDefaultConfidence
    } yield Score(score, confidence, count = count)
  }
  
}

trait DefaultActorMean extends DefaultMean {
  
  override abstract def default_score(actor: Actor, item: Item, without: Set[Review]) = {
    super.default_score(actor, item, without) orElse
      default_reviews_score(actor.reviews -- without)
  }
  
  override def toString = "actor_" + super.toString
}

trait DefaultItemMean extends DefaultMean {
  
  override abstract def default_score(actor: Actor, item: Item, without: Set[Review]) = {
    super.default_score(actor, item, without) orElse
      default_reviews_score(item.reviews -- without)
  }
  
  override def toString = "item_" + super.toString
}

trait DefaultSimilarItemMean extends DefaultScore {
  
  private val itemsCorrelation = new ItemBased(false) with Confidence.DefaultScore
  
  override def default_score(actor: Actor, item: Item, without: Set[Review]) = {
    super.default_score(actor, item, without) orElse {
      itemsCorrelation.score(actor, item, without)
    }
  }
  
  override def toString = "sitem_" + super.toString
  
}