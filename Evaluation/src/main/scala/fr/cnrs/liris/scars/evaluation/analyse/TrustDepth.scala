package fr.cnrs.liris.scars.evaluation.analyse

import fr.cnrs.liris.scars.api._
import scala.collection.mutable.Map

object TrustDepth {
  
  def draw(actors: Iterable[Actor], depth: Int) {
    actors.map{ actor =>
      actor -> draw(actor, depth)
    }.toMap
  }
  
  def draw(actor: Actor, depth: Int) = {
    new TrustDepth(actor, depth).compute
  }
}

class TrustDepth(actor: Actor, depth: Int) {
  
  var treated: Map[Actor, Int] = _
  
  def compute = {
    treated = Map[Actor, Int]()
    var actors = Set(actor)
    (1 to depth).foreach { hops =>
      actors = nextFriends(actors)
      setDepth(actors, hops)
    }
    treated
  }
  
  private def setDepth(actors: Set[Actor], hops: Int) = {
    treated ++= actors.map( _ -> hops )
  }

  private def nextFriends(actors: Set[Actor]) = {
    actors.flatMap(_.friends) -- treated.keySet
  }

}
