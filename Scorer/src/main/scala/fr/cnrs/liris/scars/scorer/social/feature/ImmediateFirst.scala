package fr.cnrs.liris.scars.scorer.social.feature

import fr.cnrs.liris.scars.api._
import fr.cnrs.liris.scars.math.Probability
import fr.cnrs.liris.scars.scorer.social._

trait ImmediateFirst extends SocialScorer {
  
  val NbFriends = 5
  val selector = Probability
  val defaultScore = Score(1)
  
  override def friends(actor: Actor, parent: Parent, item: Item, without: Set[Review]) = {
    val friends = super.friends(actor, parent, item, without)
    val (withRating, withoutRating) = friends.partition(rating(_, item, without).isDefined)
    val friendsMissing = NbFriends - withRating.size
    if (friendsMissing <= 0) {
      withRating
    } else {
      (withRating | select_some(withoutRating, friendsMissing, actor, item, without).toSet)
    }
  }
  
  def select_some(friends: Iterable[Actor], n: Int, actor: Actor, item: Item, without: Set[Review]) = {
    val weightedFriends = friends.flatMap{ friend => 
      weight(actor, friend, item, without, defaultScore).map{ w => w -> friend }
    }
    selector.selectW(weightedFriends, n).map(_._2)
  }
  
  override def toString = "imm_" + super.toString 

}
