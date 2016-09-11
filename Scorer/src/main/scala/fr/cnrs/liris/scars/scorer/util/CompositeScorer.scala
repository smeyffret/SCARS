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

class CompositeScorer(scorers: Scorer*) extends Scorer {
  
  private val lazyScorers = scorers.toStream
  
  def score(actor: Actor, item: Item, without: Set[Review]): Option[Score] = {
    lazyScorers.flatMap(_.score(actor, item, without)).headOption
  }

  override def toString = scorers.mkString("_")

}