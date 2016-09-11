package fr.cnrs.liris.scars.scorer.social.parent

import fr.cnrs.liris.scars.api._

trait Parent {
  
  type Parent
  
  def defaultParent: Parent

  def parents(parent: Parent, current: Actor): Parent

  def friends(actor: Actor, parent: Parent): Set[Actor]
  
}