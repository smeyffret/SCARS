package fr.cnrs.liris.scars.api

abstract class Item extends Identifiable with Reviews with Similarities[Item] {
  
  def name: String
  def category: Category
  
}