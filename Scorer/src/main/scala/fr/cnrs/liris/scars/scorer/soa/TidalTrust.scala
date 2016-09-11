package fr.cnrs.liris.scars.scorer.soa

import fr.cnrs.liris.scars.api._
import fr.cnrs.liris.scars.math.Math

//TODO: does not work!
class TidalTrust(depth: Int) extends ActorScorer {
  
  private val util = new Math()
  
  class Path(val source: Actor, val sink: Actor) {
    private var nodes = Seq[Path]()
    private var max = Double.MaxValue
    def add(path: Path) = {
      for {
        last <- nodes.lastOption
        trust <- last.source.trust(path.source) if (trust < max)
      }{ max = trust }
      nodes :+= path
      this
    }
    def depth = nodes.size + 1
    def trust = max
  }
  
  type TrustNetwork = Map[Actor, Set[Path]]
  
  def scores(actor: Actor, reviews: Set[Review], without: Set[Review]): Map[Review, Option[Score]] = {
    val sinks = reviews.map(_.actor)
    val friends = trustNetwork(actor, sinks)
    for {
      review <- reviews
    } yield (
      review -> score(actor, review.item, without + review, friends)
    )
  }.toMap

  def score(actor: Actor, item: Item, without: Set[Review]) = {
    val sinks = item.reviews.map(_.actor)
    val friends = trustNetwork(actor, sinks)
    score(actor, item, without, friends)
  }
  
  def score(actor: Actor, item: Item, without: Set[Review], friends: TrustNetwork) = {
    val values = for {
      review <- item.reviews
      friend = review.actor
      trust = 0.
//      (sink, paths) <- friends.view
      rating <- rating(friend, item, without)
    } yield (trust, rating.value)
    util.weighted_mean(values).map(Score(_))
  }
  
  
  def trustNetwork(source: Actor, sinks: Set[Actor]): TrustNetwork = {
    for {
      sink <- sinks
      tidalTrust = trust(source, sink, 1)
    } yield (sink -> tidalTrust)
  }.toMap
  
  def trust(source: Actor, sink: Actor, n: Int): Set[Path] = {
    if (source.trust(sink).isDefined) {
      Set(new Path(source, sink))
    } else if (n == depth){
      Set()
    } else { // n < depth
      for {
        friend <- source.friends
        path <- trust(friend, sink, n + 1)
      } yield (new Path(source, sink).add(path))
    }
  }
  
  def filterDepth(paths: List[Path]) = {
    paths.groupBy(_.depth).minBy(_._1)._2
  }
  
  def filterMax(paths: List[Path]) = {
    paths.groupBy(_.trust).maxBy(_._1)._2
  }
  
  override def toString = "TidalTrust" + depth
  
}
