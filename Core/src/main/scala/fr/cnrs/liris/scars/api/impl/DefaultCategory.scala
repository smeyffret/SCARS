package fr.cnrs.liris.scars.api.impl

import fr.cnrs.liris.scars.api._

class DefaultCategory(val id: Int, val name: String) extends Category {
  
  def addChild(child: DefaultCategory) {
    children += child
    child.parent = Some(this)
  }
  
  def addItem(item: DefaultItem) {
    items += item
    item.category = this
  }
  
  def addExpert(actor: DefaultActor) {
    experts += actor
  }
  
  var parent: Option[Category] = None
  var items = Set[Item]()
  var children = Set[Category]()
  var experts = Set[Actor]()
}