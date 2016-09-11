package fr.cnrs.liris.scars.scorer.util

import fr.cnrs.liris.scars.api._

/**
 * Created by IntelliJ IDEA.
 * Actor: smeyffret
 * Date: 9 déc. 2010
 * Time: 15:12:39
 * To change this template use File | Settings | File Templates.
 */

class LeveledScorer(scorer: Scorer, values: Seq[Double]) extends Scorer {
  
  def score(actor: Actor, item: Item, without: Set[Review]) = {
    for {
      score <- scorer.score(actor, item, without)
      rating = score.value
    } yield Score(round(rating))
  }
  
  def classes = values
  
  def round(score: Double) = {
    var index = classes.indexWhere(_ > score) // can't be 0
    if (index == -1)
      index = classes.length - 1
    val before = classes(index - 1)
    val after  = classes(index)
    nearest(score, before, after)
  }
  
  def nearest(score: Double, x: Double, y: Double) = {
    if (math.abs(score - x) < math.abs(score - y))
      x
    else
      y
  }

  override def toString = "lev_" + scorer.toString

}