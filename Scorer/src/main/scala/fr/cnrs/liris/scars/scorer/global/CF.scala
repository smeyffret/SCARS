package fr.cnrs.liris.scars.scorer.global

import fr.cnrs.liris.scars.api._
import fr.cnrs.liris.scars.math._
import fr.cnrs.liris.scars.scorer.algo.Aggregation

abstract class CF extends Scorer with Aggregation {
  
  def score(actor: Actor, item: Item, without: Set[Review]) = {
    aggregate(scoreSimilars(actor, item, without))
  }
  
  def scoreSimilars(actor: Actor, item: Item, without: Set[Review]): Iterable[Option[(Score, Double)]]
  
}
