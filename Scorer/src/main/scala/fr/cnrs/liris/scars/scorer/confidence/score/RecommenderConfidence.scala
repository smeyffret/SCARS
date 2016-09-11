package fr.cnrs.liris.scars.scorer.confidence.score

import fr.cnrs.liris.scars.api._
import fr.cnrs.liris.scars.math.Math
import fr.cnrs.liris.scars.scorer.confidence.ScoreConfidence

/**
 * @author  Simon Meyffret
 * @version 0.0.1
 * @since   scala 2.8
 * Date: 21 févr. 2011
 * Time: 14:18:18
 */

class RecommenderConfidence extends ScoreConfidence {
  
  type Weight = Double

  private val statistics = new Math()

  def confidence(scores: Iterable[(Score, Weight)]) = {
    val weightedScores = for {
        (rating, weight) <- scores
    } yield weight -> rating.confidence
    statistics.weighted_mean(weightedScores).getOrElse(0.)
  }
  
}