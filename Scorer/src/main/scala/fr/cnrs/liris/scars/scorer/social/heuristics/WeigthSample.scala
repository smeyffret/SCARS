package fr.cnrs.liris.scars.scorer.social.heuristics

import fr.cnrs.liris.scars.api._
import fr.cnrs.liris.scars.scorer.social._

trait WeightSample extends SocialScorer {
  
  override def friends_score(friendsScores: Iterable[Option[(Score, Double)]]) = {
    super.friends_score(friendsScores.toSeq.sortBy(_.getOrElse(0,0.)._2).reverse)
  }
  
  override def toString = super.toString + "_w"

}