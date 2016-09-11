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

class ConfidenceAggregation(left: ScoreConfidence*)(right: ScoreConfidence*) extends ScoreConfidence {
  
  private val util = new Math()
  
  def filter(predicate: ScoreConfidence => Boolean) = {
    new ConfidenceAggregation(left.filter(predicate): _*)(right.filter(predicate): _*)
  }
  def cancel() = new ConfidenceAggregation()()
  def toLeft() = new ConfidenceAggregation(left: _*)()
  def toRight() = new ConfidenceAggregation()(right: _*)
  def slice(fromLeft: Int = 0, untilLeft: Int = left.size)(fromRight: Int = 0, untilRight: Int = right.size) = {
    new ConfidenceAggregation(left.slice(fromLeft, untilLeft): _*)(right.slice(fromRight, untilRight): _*)
  }
  
  def confidence(scores: Iterable[(Score, Double)]) = {
    val cs = util.meanBy(left)(_.confidence(scores)).getOrElse(1.)
    val cw = util.meanBy(right)(_.confidence(scores)).getOrElse(1.)

    /*val csl = left.map(_.confidence(scores)).map(s).mkString("(","+",")")
    val cwl = right.map(_.confidence(scores)).map(s).mkString("(","+",")")
    val pretty = for {
      (score, weight) <- scores
      value = score.value
    } yield s(value) + "->" + s(score.confidence)
    println(s(cs*cw) + "=" + csl + "*" + cwl + " : " + pretty.mkString("-"))
    */
    cs * cw
  }
  implicit def s(d: Double) = "%.1f".format(d)
}
