package fr.cnrs.liris.scars.scorer.confidence.score

import fr.cnrs.liris.scars.api._
import fr.cnrs.liris.scars.scorer.confidence._

/**
 * @author  Simon Meyffret
 * @version 0.0.1
 * @since   scala 2.8
 * Date: 21 févr. 2011
 * Time: 14:18:18
 */


/**
 * Multiplie la confidence d'un score calculé par une autre confidence 
 * basée sur la variance de la moyenne des scores des amis. Cf TrustWalker
 */
class VarianceConfidence extends ScoreConfidence with Variance {
  
  def confidence(scores: Iterable[(Score, Double)]) = {
    variance_confidence(scores.map{_._1.value})
  }

}

/**
 * Calcule une variance pondérée par le poids des scores (bug)
 * Pas pertinent d'après mes tests
 */
class WeightedVarianceConfidence extends VarianceConfidence {
  
  override def confidence(scores: Iterable[(Score, Double)]) = {
    val weightedScores = for {
      (rating, weight) <- scores
    } yield (weight -> rating.value)
    weighted_variance_confidence(weightedScores)
  }

}
