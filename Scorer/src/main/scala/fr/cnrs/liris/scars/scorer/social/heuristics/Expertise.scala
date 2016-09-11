package fr.cnrs.liris.scars.scorer.social.heuristics

import fr.cnrs.liris.scars.api._
import fr.cnrs.liris.scars.scorer.social._

trait Expertise extends SocialScorer with DefaultWeight {

//  val NoneExpertise = 0.1
//  val AdvisorExpertise = 0.6
//  val ReviewerExpertise = 0.8
//  val LeaderExpertise = 1.0
  
  override def friends(actor: Actor, parent: Parent, item: Item, without: Set[Review]) = {
    val experts = item.category.experts
    val friends = super.friends(actor, parent, item, without)
    val inter = experts & friends
    if (inter.isEmpty)
      friends
    else
      inter
  }

//  override def weight(actor: Actor, friend: Actor, item: Item, without: Set[Review]) = {
//    val xp = expertise(friend, item)
//    super.weight(actor, friend, item, without).map( _ * xp )
//  }
//
//  def expertise(actor: Actor, item: Item) = {
//      val category = item.category
//      val top = actor.expertises.find { topCategory =>
//        topCategory.isAncestorOf(category) || category.isAncestorOf(topCategory)
//      }
//      top.flatMap(actor.expertise).map(expertWeight).getOrElse(NoneExpertise)
//  }
//  
//  private def expertWeight(weight: Expertise.Value) = weight match {
//    case Expertise.Advisor => AdvisorExpertise
//    case Expertise.Reviewer => ReviewerExpertise
//    case Expertise.Leader => LeaderExpertise
//    case _ => NoneExpertise
//  }

  override def toString = super.toString + "Xp"

}