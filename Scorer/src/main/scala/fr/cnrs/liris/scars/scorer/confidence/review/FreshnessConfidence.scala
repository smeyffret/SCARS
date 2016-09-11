package fr.cnrs.liris.scars.scorer.confidence.review

import fr.cnrs.liris.scars.api._
import fr.cnrs.liris.scars.util.Time
import fr.cnrs.liris.scars.math.Math
import fr.cnrs.liris.scars.scorer.confidence.ReviewConfidence
import java.util.Date

/**
 * @author  Simon Meyffret
 * @version 0.0.1
 * @since   scala 2.8
 * Date: 21 févr. 2011
 * Time: 14:18:18
 */


class FreshnessConfidence(val today: Date = new Date(), 
                            val lambda: Int = 5,
                            val scale: Time.Unit = Time.MINUTE) extends ReviewConfidence {
  
  private val functions = new Math()
  private val alpha = 1. / (2 * functions.sigmoid(lambda))
  private val beta = 0.5
  
  private def freshness(age: Double): Double = {
    alpha * functions.sigmoid(lambda - age) + beta
  }
  
  private def freshness(date: Date): Double = {
    val age_ms = today.getTime() - date.getTime()
    val age = scale.fromMillis(age_ms)
    freshness(age)
  }

  def confidence(review: Review) = review.date match {
    case Some(date) => freshness(date)
    case None => 1.
  }
  
}