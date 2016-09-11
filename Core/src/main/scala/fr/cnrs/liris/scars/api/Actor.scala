package fr.cnrs.liris.scars.api

object Expertise extends Enumeration {
  val Advisor  = Value("advisor")
  val Reviewer = Value("top reviewer")
  val Leader   = Value("category leads")
}

abstract class Actor extends Identifiable with Reviews with Similarities[Actor] {
  
  def rank: Option[Int]
  def friends: Set[Actor]
  def followers: Set[Actor]
  def expertises: Set[Category]
  
  def meanRating: Option[Double]
  def meanRating(without: Set[Review]): Option[Double]
  
  def friendsCount: Int
  def reviewsCount: Int
  
  def trust(other: Actor): Option[Double]
  def review(item: Item): Option[Review]
  def expertise(category: Category): Option[Expertise.Value]
  
}