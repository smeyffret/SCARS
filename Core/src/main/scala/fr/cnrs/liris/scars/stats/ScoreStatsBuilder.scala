package fr.cnrs.liris.scars.stats

import fr.cnrs.liris.scars.math.Math
import fr.cnrs.liris.scars.api.Score

/**
 * @author  Simon Meyffret
 * @version 0.0.1
 * @since   scala 2.8
 * Date: 10 déc. 2010
 * Time: 14:02:26
 */

class ScoreStatsBuilder(allScores: Iterable[(Any, Any, Double, Option[Score])], otherValues: Iterable[(Any, Any, Double)], values: Stats.Value*) {

  val util = new Math()
  

  val allValues = allScores.view.map{ case (a,i,r,s) => (a,i,r,s.map(_.value))}
  val statsBuilder = new StatsBuilder(allValues, otherValues, values: _*)
  
  lazy val actors = util.mean(allScores.map(_._4).flatten.map(_.actors.toDouble)).getOrElse(0.0).toInt
  lazy val scores = util.mean(allScores.map(_._4).flatten.map(_.count.toDouble)).getOrElse(0.0).toInt
  lazy val time = util.mean(allScores.map(_._4).flatten.map(_.time)).getOrElse(0.0)
  
  //lazy val confidences = allScores.map(_._4).map(_.map(_.confidence))
  //lazy val errors      = statsBuilder.errors().map(Some(_))
  //lazy val errorsVSconfidences = errors zip confidences
  lazy val confidenceValues = for {
    (_, _, rating, Some(score)) <- allScores
    error = rating - score.value
    confidence = score.confidence
  } yield (Some(error.abs) -> Some(confidence))
//  lazy val sconfidenceValues = confidenceValues.collect {
//    case (Some(error) , Some(confidence)) => error -> confidence
//  }
//  println(sconfidenceValues.mkString("\n"))
  lazy val confidence = statsBuilder.pearsonCorrelationCoefficient(confidenceValues) * -1
  lazy val sconfidence = statsBuilder.spearmanRankCorrelationCoefficient(confidenceValues) * -1
  
  var results = statsBuilder.buildResults(metricValue orElse statsBuilder.metricValue)

  def metricValue: PartialFunction[Stats.Value, Double] = {
    case Stats.actors      => actors
    case Stats.scores      => scores
    case Stats.time        => time
    case Stats.confidence  => confidence
    case Stats.sconfidence => sconfidence
  }

  def build() = new Stats(statsBuilder.results ++ results)


}
