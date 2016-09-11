package fr.cnrs.liris.scars.scorer.social.feature

import fr.cnrs.liris.scars.api._
import fr.cnrs.liris.scars.scorer.social._

/**
 * @author  Simon Meyffret
 * @version 0.0.1
 * @since   scala 2.8
 * Date: 21 févr. 2011
 * Time: 14:18:18
 */


trait BreadthPropagation extends SocialScorer {
  
  override def score(actor: Actor, item: Item, without: Set[Review]) = {
    val scores = for {
      n <- (1 to depth).view
      score <- social_score(actor, item, defaultParent, without, hops, n)
    } yield score
    scores.headOption
  }

  override def toString = "breadth_" + super.toString

}