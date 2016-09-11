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


/**
 * La confidence dépend de la taille de l'échantillon
 * autour de 4 ou plus -> proche de 1
 * à 1 -> 0.5
 */
class SizeConfidence(offset: Int = -1) extends ScoreConfidence {
  
  private val functions = new Math()

  def confidence(scores: Iterable[(Score, Double)]) = {
    val size = scores.size
    functions.sigmoid(size + offset)
  }
  
}