package fr.cnrs.liris.scars.api

/**
 * Return always None, for testing purpose
 * 
 * @author  Simon Meyffret
 * @version 0.0.1
 * @since   scala 2.8
 * Date: 9 déc. 2010
 * Time: 12:27:20
 */

object NoneScorer extends Scorer {
  
  def score(actor: Actor, item: Item, without: Set[Review]) = None
  
}