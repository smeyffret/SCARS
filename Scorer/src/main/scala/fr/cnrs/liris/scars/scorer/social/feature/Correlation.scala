package fr.cnrs.liris.scars.scorer.social.feature

import fr.cnrs.liris.scars.api._
import fr.cnrs.liris.scars.math._
import fr.cnrs.liris.scars.scorer.social._
import fr.cnrs.liris.scars.scorer.algo

trait Correlation extends SocialScorer {
  
  //TODO: expérimenter et justifier une valeur cohérente
  val defaultSimilarity = Some(0.5)
  val negativeSimilarity = Some(0.1)
  protected val correlation = algo.Correlation.actors(useCache = false)

  override def weight(actor: Actor, friend: Actor, item: Item, without: Set[Review], score: Score): Option[Double] = {
    similarity(actor, friend, without) match {
      case None => defaultSimilarity
      case Some(x) => if (x < 0) negativeSimilarity else Some(x)
    }
  }

  def similarity(actor: Actor, friend: Actor, without: Set[Review]) = {
    correlation.similarity(actor, friend, without)
  }
  
  override def toString = "cor_" + super.toString

}

trait CachedCorrelation {
  self: Correlation =>
    override protected val correlation = algo.Correlation.actors(useCache = true)
}
