package fr.cnrs.liris.scars.stats

import fr.cnrs.liris.scars.api.Score
import scala.collection.SortedSet

/**
 * @author  Simon Meyffret
 * @version 0.0.1
 * @since   scala 2.8
 * Date: 10 déc. 2010
 * Time: 14:02:26
 */

object Stats extends Enumeration {

  val
    total, atotal, cov, acov,
    mae, amae, rmse, rae, rrse, wae, rwse,
    srcc, asrcc, pcc, apcc,
    actors, scores, time, confidence, sconfidence = Value
  var defaultMetrics = SortedSet(total, cov, mae, rmse, time)
  
  val intMetrics = Set(Stats.total, Stats.atotal, Stats.actors, Stats.scores)
  val percentMetrics = Set(Stats.cov, Stats.acov)

  var SEPARATOR = " "
  def titles(sep: String = SEPARATOR) = defaultMetrics.mkString(sep)

  def apply(allValues: Iterable[(Any, Any, Double, Option[Double])]): Stats = {
    apply(allValues, Nil, defaultMetrics.toSeq: _*)
  }
  def apply(allValues: Iterable[(Any, Any, Double, Option[Double])], otherValues: Iterable[(Any, Any, Double)]): Stats = {
    apply(allValues, otherValues, defaultMetrics.toSeq: _*)
  }
  def apply(allValues: Iterable[(Any, Any, Double, Option[Double])], metrics: Stats.Value*): Stats = {
    new StatsBuilder(allValues, Nil, metrics: _*).build()
  }
  def apply(allValues: Iterable[(Any, Any, Double, Option[Double])], otherValues: Iterable[(Any, Any, Double)], metrics: Stats.Value*): Stats = {
    new StatsBuilder(allValues, otherValues, metrics: _*).build()
  }

  def scores(allValues: Iterable[(Any, Any, Double, Option[Score])]): Stats = {
    scores(allValues, Nil, defaultMetrics.toSeq: _*)
  }
  def scores(allValues: Iterable[(Any, Any, Double, Option[Score])], otherValues: Iterable[(Any, Any, Double)]): Stats = {
    scores(allValues, otherValues, defaultMetrics.toSeq: _*)
  }
  def scores(allValues: Iterable[(Any, Any, Double, Option[Score])], metrics: Stats.Value*): Stats = {
    new ScoreStatsBuilder(allValues, Nil, metrics: _*).build()
  }
  def scores(allValues: Iterable[(Any, Any, Double, Option[Score])], otherValues: Iterable[(Any, Any, Double)], metrics: Stats.Value*): Stats = {
    new ScoreStatsBuilder(allValues, otherValues, metrics: _*).build()
  }
}


case class Stats(results: Map[Stats.Value, Double]) {
  
  def get(metric: Stats.Value) = results.get(metric)

  override def toString = results.toList.sortBy(_._1).map { case (metric, value) =>
      metric match {
        case m if (Stats.intMetrics.contains(m))     => value.toInt
        case m if (Stats.percentMetrics.contains(m)) => "%.2f%%".format(value)
        case _                                       => "%.3f".format(value)
      }
    }.mkString(Stats.SEPARATOR)

}
