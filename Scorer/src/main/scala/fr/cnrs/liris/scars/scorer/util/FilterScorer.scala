package fr.cnrs.liris.scars.scorer.util

import fr.cnrs.liris.scars.api._

/**
 * A composite scorer which executes scores until a non None score is computed
 * The scorers are used in the ordered of the parameters in the constructor.
 *
 * @author  Simon Meyffret
 * @version 0.0.1
 * @since   scala 2.8
 * Date: 30 nov. 2010
 * Time: 13:45:38
 *
 * @param scorers list of scorers in the composite scorer
 */

class FilterScorer(scorers: Scorer*) extends Scorer {
  
  require(scorers.size >= 2)
  
  private val lazyScorers = scorers.toStream
  private val filterCount = scorers.size - 1
  
  def score(actor: Actor, item: Item, without: Set[Review]) = {
    val scores = lazyScorers.map(_.score(actor, item, without))
    if (scores.take(filterCount).forall(_.isDefined)) {
      scores.last
    } else {
      None
    }
  }

  override def toString = scorers.mkString(">")

}