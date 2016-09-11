package fr.cnrs.liris.scars.scorer.social.heuristics

import fr.cnrs.liris.scars.api._
import fr.cnrs.liris.scars.scorer.social._

trait GlobalExpertise extends SocialScorer with DefaultWeight {

  override def friends(actor: Actor, parent: Parent, item: Item, without: Set[Review]) = {
    val experts = item.category.experts
    val friends = super.friends(actor, parent, item, without)
    experts ++ friends
  }

  override def toString = super.toString + "GloXp"

}