package fr.cnrs.liris.scars.scorer.social.heuristics

import fr.cnrs.liris.scars.api._
import fr.cnrs.liris.scars.scorer.social._

trait DefaultWeight extends SocialScorer {

  abstract override def weight(actor: Actor, friend: Actor, item: Item, without: Set[Review], score: Score) = {
    super.weight(actor, friend, item, without, score) orElse Some(1)
  }

}