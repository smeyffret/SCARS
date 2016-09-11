package fr.cnrs.liris.scars.scorer.confidence

import fr.cnrs.liris.scars.math.Math

object Variance {
  
  val MaxRange = 4 // dépend du dataset !
  val DefaultVariance = 0.
  def MaxSigma = MaxRange * MaxRange / 4
  
}
  
trait Variance {
  import Variance._
  private val statistics = new Math()
  
  def variance_confidence(series: Iterable[Double]) = {
    confidence(statistics.variance(series))
  }
  
  /// (Weight, Value)
  def weighted_variance_confidence(series: Iterable[(Double, Double)]) = {
    confidence(statistics.weighted_variance(series))
  }
  
  def confidence(variance: Option[Double]): Double = {
    confidence(variance getOrElse DefaultVariance)
  }

  def confidence(variance: Double) = {
    1 - variance / MaxSigma
  }

}