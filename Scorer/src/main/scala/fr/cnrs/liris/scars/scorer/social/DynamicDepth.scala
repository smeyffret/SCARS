package fr.cnrs.liris.scars.scorer.social

import fr.cnrs.liris.scars.api._
import scala.collection.mutable.Map

class DynamicDepth(builder: (Int => Scorer)) extends Scorer {
  
  val coldDepth = 4
  val mediumDepth = 3
  val heavyDepth = 3
  val scorers = Map[Int, Scorer]()
  
  def score(actor: Actor, item: Item, without: Set[Review]) = {
    getScorer(actor).score(actor, item, without)
  }
  
  def getScorer(actor: Actor) = {
    val depth = computeDepth(actor)
    getOrBuild(depth)
  }
  
  def computeDepth(actor: Actor) = {
    val friendsNb = actor.friendsCount
    if (friendsNb <= 4)
      coldDepth
    else if (friendsNb <= 10)
      mediumDepth
    else
      heavyDepth
  }
  
  def getOrBuild(depth: Int) = {
    scorers.getOrElseUpdate(depth, builder(depth))
  }
  
  override def toString = "DynCoTCoDepthCold" 

}
