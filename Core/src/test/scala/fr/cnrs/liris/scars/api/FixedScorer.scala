package fr.cnrs.liris.scars.api

/**
 * A scorer which return a fixed value given in the constructor
 *
 * @author  Simon Meyffret
 * @version 0.0.1
 * @since   scala 2.8
 * Date: 29 nov. 2010
 * Time: 16:34:17
 *
 * @param value the value always returned by the scorer
 */

class FixedScorer(rating: Double) extends Scorer {
  
  val score = Score(rating)

  def score(actor: Actor, item: Item, without: Set[Review]) = Some(score)

  override def toString = "fixed_" + rating
  
}
