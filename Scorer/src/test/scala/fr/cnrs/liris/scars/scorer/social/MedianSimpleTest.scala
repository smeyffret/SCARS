package fr.cnrs.liris.scars.scorer.social

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import fr.cnrs.liris.scars.api._
import fr.cnrs.liris.scars.scorer.social.impl._
import fr.cnrs.liris.scars.scorer.social.feature._
import fr.cnrs.liris.scars.api.Conversion._
import fr.cnrs.liris.scars.test.Conversion._
import parent.SingleParent
import fr.cnrs.liris.scars.test.SocialScorerTool

/**
 * @author  Simon Meyffret
 * @version 0.0.1
 * @since   scala 2.8
 * Date: 9 déc. 2010
 * Time: 12:32:04
 */

@RunWith(classOf[JUnitRunner])
class MedianSimpleTest extends FunSuite with ShouldMatchers {

  val scorer = new RecSocialScorer(1, 1) with SingleParent with Median with Trust

  test("An int weighted collection should return the correct median") {
    val scores = buildScores(2 -> 1, 3 -> 2, 5 -> 7, 6 -> 1, 8 -> 5, 9 -> 2)
    val weights = scores.map(_._2)
    val (summed, medium) = scorer.sum(weights)
    summed should be(List[Double](1, 3, 10, 11, 16, 18))
    medium should be(9)
    scorer.findMedian(scores) should be(Some(Score(5)))
  }
  
  test("A double weighted collection should return the correct median") {
    val scores = buildScores(2 -> 0.1, 3 -> 0.2, 5 -> 0.7, 6 -> 0.1, 8 -> 0.5, 9 -> 0.2)
    val weights = scores.map(_._2)
    val (summed, medium) = scorer.sum(weights)
    compare(summed, List[Double](0.1, 0.3, 1.0, 1.1, 1.6, 1.8))
    medium should be(0.9)
    scorer.findMedian(scores) should be(Some(Score(5)))
  }
  
  def compare(weights: Seq[Double], values: Seq[Double]) {
    weights.zip(values).foreach { case (weight, value) =>
      weight should be (value plusOrMinus 0.00001)
    }
  }
  
  implicit def intToDouble(pair: (Int, Int)) = (pair._1.toDouble, pair._2.toDouble)
  implicit def intDoubleToDouble(pair: (Int, Double)) = (pair._1.toDouble, pair._2)
  
  def buildScores(pairs: (Double, Double)*) = {
    pairs.map{ case (r, w) => Score(r) -> w }.toList
  }


}
