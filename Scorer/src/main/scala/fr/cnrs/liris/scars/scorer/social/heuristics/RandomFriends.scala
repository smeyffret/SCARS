package fr.cnrs.liris.scars.scorer.social.heuristics

import fr.cnrs.liris.scars.api._
import fr.cnrs.liris.scars.scorer.social._
import scala.collection.mutable.LinkedHashSet
import scala.util.Random

trait RandomFriends extends SocialScorer {
  
  override def friends(actor: Actor, parent: Parent, item: Item, without: Set[Review]) = {
    val seqFriends = LinkedHashSet() ++ super.friends(actor, parent, item, without)
    Random.shuffle(seqFriends).toSet
  }

}