package fr.cnrs.liris.scars.scorer.social.parent

import fr.cnrs.liris.scars.api._

trait Orphan extends Parent {
  
  type Parent = Unit
  
  val defaultParent: Parent = null

  def parents(parent: Parent, current: Actor) = defaultParent

  def friends(actor: Actor, parent: Parent) = actor.friends

}