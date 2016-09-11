package fr.cnrs.liris.scars.stats

import fr.cnrs.liris.scars.math.Math

/**
 * @author  Simon Meyffret
 * @version 0.0.1
 * @since   scala 2.8
 * Date: 10 déc. 2010
 * Time: 14:02:26
 */

class StatsBuilder(allValues: Iterable[(Any, Any, Double, Option[Double])], otherValues: Iterable[(Any, Any, Double)], values: Stats.Value*) {

  val util = new Math()

  val definedValues = allValues.filter(_._4.isDefined)
  /*collect{
    case (actor, item, rating, Some(score)) => (actor, item, rating, score)
  }*/

  val total = allValues.size
  val classified = definedValues.size

  lazy val actorAllValues= allValues.groupBy(_._1)
  lazy val actorTotal = actorAllValues.size
  lazy val actorValues = definedValues.groupBy(_._1)
  lazy val actorCount = countActorValues(actorAllValues, actorValues)
  lazy val actorMultValues = actorValues.filter(_._2.size > 1).mapValues(getScores)
  lazy val actorValuesCount = actorValues.size
  lazy val actorValuesMean = compute(actorValues.map(_._2.size).sum.toDouble / actorValuesCount)

  lazy val itemRatings = (allValues.view.map(t => (t._1, t._2, t._3)) ++ otherValues).groupBy(_._2).toMap
  lazy val itemValues = compute(definedValues.groupBy(_._2))
  lazy val itemValuesCount = itemValues.size
  lazy val itemValuesMean = compute(itemValues.map(_._2.size).sum.toDouble / itemValuesCount)

  lazy val ratings = compute(definedValues.map(_._3))
  lazy val ratingsMean = compute(ratings.sum / classified)
  lazy val ratingsMeanDiff = compute(ratings.map(_ - ratingsMean))

  var results = buildResults(metricValue)
  def buildResults(function: PartialFunction[Stats.Value, Double]) = 
    if  (classified == 0) {
      Map[Stats.Value, Double]()
    } else {
      Map(values.filter(function isDefinedAt _).map(x => x -> x): _*).
        mapValues(function)
    }

  def metricValue: PartialFunction[Stats.Value, Double] = {
    case Stats.total => total
    case Stats.atotal=> actorTotal
    case Stats.cov   => (classified * 100.0) / total
    case Stats.acov  => (actorValuesCount * 100.0) / actorTotal
    case Stats.mae   => meanAbsoluteError(errors())
    case Stats.amae  => actorStats[(Any,Any,Double,Option[Double])]( x => meanAbsoluteError(errors(x)), actorValues)
    case Stats.rmse  => rootMeanSquareError(errors())
    case Stats.rae   => relativeAbsoluteError(errors())
    case Stats.rrse  => rootRelativeSquaredError(errors())
    case Stats.wae   => weightedAbsoluteError
    case Stats.rwse  => rootWeightedSquaredError
    case Stats.srcc  => spearmanRankCorrelationCoefficient(getScores(definedValues))
    case Stats.asrcc => actorStats(spearmanRankCorrelationCoefficient, actorMultValues)
    case Stats.pcc   => pearsonCorrelationCoefficient(getScores(definedValues))
    case Stats.apcc  => actorStats(pearsonCorrelationCoefficient, actorMultValues)
  }

  def build() = new Stats(results)


  protected def compute(f: => Double, todo: Boolean = true) = if (classified > 0 && todo) f else Double.NaN

  protected def compute[T](f: => Iterable[T]) = if (classified > 0) f else Nil

  
  protected def countActorValues[T](actorAllValues: Map[Any, Iterable[T]],
                                actorValues: Map[Any, Iterable[T]]) = {
    actorAllValues.map{ case (actor, allValues) =>
        val valueCount = actorValues.get(actor).map{_.size}.getOrElse(0).toDouble
        valueCount / allValues.size
    }.sum
  }
  
  def errors(values: Iterable[(Any, Any, Double, Option[Double])] = definedValues) = {
    values.view.map{v => v._3 - v._4.get}
  }

  protected def meanAbsoluteError(errors: Iterable[Double]): Double = {
    errors.map(math.abs).sum / errors.size
  }

  protected def rootMeanSquareError(errors: Iterable[Double]): Double = {
    math.sqrt(errors.map(math.pow(_,2)).sum / errors.size)
  }

  protected def relativeAbsoluteError(errors: Iterable[Double]): Double = {
    val numerator = errors.foldLeft(0.0){(sum, diff) => sum + math.abs(diff)}
    val denominator = ratingsMeanDiff.foldLeft(0.0){(sum, diff) => sum + math.abs(diff)}
    numerator / denominator
  }

  protected def rootRelativeSquaredError(errors: Iterable[Double]): Double = {
    val numerator = errors.foldLeft(0.0){(sum, diff) => sum + math.pow(diff, 2)}
    val denominator = ratingsMeanDiff.foldLeft(0.0){(sum, diff) => sum + math.pow(diff, 2)}
    math.sqrt(numerator / denominator)
  }

  protected def weightedAbsoluteError(): Double = {
    val numerator = definedValues.collect{case (_, item, rating, Some(score)) =>
      (rating - score) * (rating - mean(item).getOrElse(rating))
    }.foldLeft(0.0){(sum, diff) => sum + math.abs(diff)}
    val denominator = classified //diffMean.foldLeft(0.0){(sum, diff) => sum + math.abs(diff)}
    numerator / denominator
  }

  protected def rootWeightedSquaredError(): Double = {
    val numerator = definedValues.collect{case (_, item, rating, Some(score)) =>
      (rating - score) * (rating - mean(item).getOrElse(rating))
    }.foldLeft(0.0){(sum, diff) => sum + math.pow(diff, 2)}
    val denominator = classified //diffMean.foldLeft(0.0){(sum, diff) => sum + math.pow(diff, 2)}
    math.sqrt(numerator / denominator)
  }

  def mean(item: Any) = {
    val ratings = itemRatings.get(item).getOrElse(Nil)
    if (ratings.isEmpty)
      None
    else
      Some(ratings.map(_._3).sum.toDouble / ratings.size)
  }

  def actorStats[T](function: Iterable[T] => Double,
                actorValues: Iterable[(Any, Iterable[T])]) = {
    val values = actorValues.view.map{case (_, list) =>
      function(list)
    }.filterNot(_.isNaN).force
    if(values.isEmpty){
          Double.NaN
    } else {
      values.sum / values.size
    }
  }

  def pearsonCorrelationCoefficient(list: Iterable[(Option[Double], Option[Double])]): Double = {
    util.pearsonCorrelationCoefficient(list).getOrElse(Double.NaN)
  }

  def spearmanRankCorrelationCoefficient(list: Iterable[(Option[Double], Option[Double])]): Double = {
    val (ratings, scores) = list.unzip
    val ratingsRank = util.valuesToRanks(ratings)
    val scoresRank = util.valuesToRanks(scores)
    util.pearsonCorrelationCoefficient(ratingsRank zip scoresRank).getOrElse(Double.NaN)
  }


  def getScores(list: Iterable[(Any, Any, Double, Option[Double])]) = list.map{
    case (actor, item, rating, score) => (Some(rating), score)
  }

}
