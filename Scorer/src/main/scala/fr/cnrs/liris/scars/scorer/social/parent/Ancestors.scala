package fr.cnrs.liris.scars.scorer.social.parent

import fr.cnrs.liris.scars.api._

trait Ancestors extends Parent {
  
  type Parent = Set[Actor]
  
  def defaultParent: Parent = Set()

  def parents(parent: Parent, current: Actor): Parent = parent + current

  def friends(actor: Actor, parent: Parent) = actor.friends &~ parent

}