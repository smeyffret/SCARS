package fr.cnrs.liris.scars.scorer.confidence

import fr.cnrs.liris.scars.api._
import fr.cnrs.liris.scars.util.Time
import fr.cnrs.liris.scars.scorer.confidence.score._
import fr.cnrs.liris.scars.scorer.confidence.review._

/**
 * @author  Simon Meyffret
 * @version 0.0.1
 * @since   scala 2.8
 * Date: 21 févr. 2011
 * Time: 14:18:18
 */

object Confidence {
  
  def toDate(date: String) = new java.text.SimpleDateFormat("yyyy-MM-dd").parse(date)
  val freshness_today_appolicious = toDate("2012-02-27");
  val freshness_today_red         = toDate("2011-06-16");
  var freshness_today = freshness_today_appolicious
  var freshness_lambda = 5
  var freshness_scale = Time.MONTH
  
  var size_offset = -1
  
  def score(): ScoreConfidence = {
    new ConfidenceAggregation(
      new SizeConfidence(size_offset)
    )(
      new WeightConfidence(), 
      new RecommenderConfidence(),
      new WeightedVarianceConfidence()
    )
  }
  
  def defaultScore(): ScoreConfidence = {
    new ConfidenceAggregation(
      new SizeConfidence(size_offset)
    )(
      new VarianceConfidence()
    )
  }
  
  def rating(): ReviewConfidence = {
    new FreshnessConfidence(today = freshness_today, 
                            lambda = freshness_lambda, 
                            scale = freshness_scale)
  }
  
  def walker(): ScoreConfidence = new VarianceConfidence()

  def none() = new ScoreConfidence with ReviewConfidence {
    def confidence(scores: Iterable[(Score, Double)]) = 1.0
    def confidence(review: Review) = 1.0
  }

}

trait ScoreConfidence {
  def confidenceSome(scores: Iterable[Option[(Score, Double)]]) = confidence(scores.flatten)
  def confidence(scores: Iterable[(Score, Double)]): Double
}

trait ReviewConfidence {
  def confidenceSome(review: Option[Review]) = review map (confidence) getOrElse 1.
  def confidence(review: Review): Double
}
