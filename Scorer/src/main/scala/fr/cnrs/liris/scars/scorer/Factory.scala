package fr.cnrs.liris.scars.scorer

import fr.cnrs.liris.scars.api._

object Factory {
  def singleton(s: => Scorer) = new SingletonFactory(s)
  def apply(s: => Scorer): Factory = apply(s, () => s)
  def apply(instance: Scorer, s: () => Scorer) = {
    instance match {
      case i: ItemScorer => 
        new ItemFactory(i, s.asInstanceOf[() => ItemScorer])
      case a: ActorScorer => 
        new ActorFactory(a, s.asInstanceOf[() => ActorScorer])
      case _ => 
        new ReviewFactory(instance, s)
    }
  }
}

sealed trait Factory {
  type S <: Scorer
  val instance: S
  private var firstInstance = true
  val builder: () => S
  
  def apply(): S = {
    if (firstInstance) {
      firstInstance = false
      instance
    } else {
      builder()
    }
  }
  
  override def toString = instance.toString
  
  def compose[C <: Scorer](f: S => C) = {
    Factory(f(apply()))
  }
}

class ReviewFactory(val instance: Scorer, val builder: () => Scorer) extends Factory{
  type S = Scorer
}
class SingletonFactory(instance: Scorer) extends ReviewFactory(instance, () => instance)

class ItemFactory(val instance: ItemScorer, val builder: () => ItemScorer) extends Factory {
  type S = ItemScorer
}
class ActorFactory(val instance: ActorScorer, val builder: () => ActorScorer) extends Factory {
  type S = ActorScorer
}