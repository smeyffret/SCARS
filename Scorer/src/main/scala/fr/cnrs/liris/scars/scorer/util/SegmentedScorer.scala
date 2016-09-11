package fr.cnrs.liris.scars.scorer.util

import fr.cnrs.liris.scars.api._

/**
 * Created by IntelliJ IDEA.
 * Actor: smeyffret
 * Date: 9 déc. 2010
 * Time: 15:12:39
 * To change this template use File | Settings | File Templates.
 */

class SegmentedScorer(scorer: Scorer, values: Seq[Double]) extends LeveledScorer(scorer, values) {
  
  //TODO: ce scorer n'est pas générique, il part du principe que chaque valeurs sont uniformément réparties
  val min = values.head
  val max = values.last
  private val range = max - min
  private val nbRates = (range + 1).ceil.toInt
  private val delta = range.toDouble / nbRates
  
  override def round(score: Double): Double = {
    (1 to nbRates) foreach { i =>
      if (score <= min + i * delta)
        return min + i - 1
    }
    return super.round(score)
  }

  override def toString = "seg_" + scorer.toString

}