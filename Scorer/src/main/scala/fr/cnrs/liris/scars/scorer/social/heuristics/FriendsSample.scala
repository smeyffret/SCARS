package fr.cnrs.liris.scars.scorer.social.heuristics

import fr.cnrs.liris.scars.api._
import fr.cnrs.liris.scars.scorer.social._

trait FriendsSample extends SocialScorer with RandomFriends {
  
  val friends_max: Int

  override def friends(actor: Actor, parent: Parent, item: Item, without: Set[Review]) = {
    super.friends(actor, parent, item, without).take(friends_max)
  }

  override def toString = super.toString + "_max" + friends_max

}