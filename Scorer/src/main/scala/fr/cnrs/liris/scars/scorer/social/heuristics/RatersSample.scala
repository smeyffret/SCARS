package fr.cnrs.liris.scars.scorer.social.heuristics

import fr.cnrs.liris.scars.api._
import fr.cnrs.liris.scars.scorer.social._

trait RatersSample extends SocialScorer with RandomFriends {
  
  val raters_max: Int

  override def friends_score(friendsScores: Iterable[Option[(Score, Double)]]) = {
    super.friends_score(friendsScores.filter(_.isDefined).take(raters_max))
  }
  
  override def toString = super.toString + "_rmax" + raters_max

}