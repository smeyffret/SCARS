package fr.cnrs.liris.scars.scorer.social.feature

import fr.cnrs.liris.scars.api._
import fr.cnrs.liris.scars.scorer.social._
import fr.cnrs.liris.scars.scorer.algo

trait CoTCoD extends SocialScorer with SocialConfidence with Correlation with Trust {
  
  override def weight(actor: Actor, friend: Actor, item: Item, without: Set[Review], score: Score): Option[Double] = {
    for {
      trust <- super[Trust].weight(actor, friend, item, without, score)
      correlation <- super[Correlation].weight(actor, friend, item, without, score)
      confidence <- super[SocialConfidence].weight(actor, friend, item, without, score)
    } yield weight(trust, correlation, confidence)
  }
  
  def weight(trust: Double, correlation: Double, confidence: Double): Double = {
    trust * (correlation + confidence) / 2
  }

  override def toString = "CoTCoD" + depth

}