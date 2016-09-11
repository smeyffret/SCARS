package fr.cnrs.liris.scars.scorer.social.parent

import fr.cnrs.liris.scars.api._

trait SingleParent extends Parent {
  
  type Parent = Actor
  
  def defaultParent: Parent = null

  def parents(parent: Parent, current: Actor): Parent = current

  def friends(actor: Actor, parent: Parent) = actor.friends - parent

}