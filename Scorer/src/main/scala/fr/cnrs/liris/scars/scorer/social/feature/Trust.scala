package fr.cnrs.liris.scars.scorer.social.feature

import fr.cnrs.liris.scars.api._
import fr.cnrs.liris.scars.scorer.social._

trait Trust extends SocialScorer {
  
  override def weight(actor: Actor, friend: Actor, item: Item, without: Set[Review], score: Score): Option[Double] = {
    actor.trust(friend)
  }

  override def toString = "trust_" + super.toString

}