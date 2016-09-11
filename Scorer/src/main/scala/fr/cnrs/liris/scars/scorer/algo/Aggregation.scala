package fr.cnrs.liris.scars.scorer.algo

import fr.cnrs.liris.scars.api._
import fr.cnrs.liris.scars.math.Math

trait Aggregation {
  
  self: Scorer =>
  
  private val util = new Math()
  
  def aggregate(scores: Iterable[Option[(Score, Double)]]) = {
    var count = 0
    var actors = 0
    val flattenedScores = for {
      pair <- scores
      (score, weight) <- pair
    } yield {
      count += score.count
      actors += score.actors
      weight -> score.value
    }
    //actors += scores.size - flattenedScores.size
    for {
      mean <- util.weighted_mean(flattenedScores)
    } yield Score(mean, count = count, actors = actors)
  }
  
}