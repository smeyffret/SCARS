package fr.cnrs.liris.scars.api.impl

import fr.cnrs.liris.scars.api._

class DefaultItem(val id: Int, val name: String) extends Item {
  
  def add(review: Review) = {
    reviews += review
  }

  def addSimilarity(other: Item, correlation: Double) = {
    similarities += (other -> correlation)
  }
  
  def similarity(other: Item) = similarities get other

  private var similarities = Map[Item, Double]()
  def similars = similarities.keySet
  var reviews = Set[Review]()
  var category: Category = _
}