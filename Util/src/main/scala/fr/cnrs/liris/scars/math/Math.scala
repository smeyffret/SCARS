package fr.cnrs.liris.scars.math

/**
 * @author  Simon Meyffret
 * @version 0.0.1
 * @since   scala 2.8
 * Date: 3 fev. 2011
 * Time: 13:06:02
 */

import scala.collection.immutable.SortedMap
import fr.cnrs.liris.scars.util.Condition

object Math extends Math

class Math extends Condition {
  //TODO: séparer cette classe en sous classes indépendantes
  
  def sigmoid(x: Double) = {
    1 / (1 + math.exp(-x))
  }
  
  def restrict(amt: Double,
            floor: Double = Double.NegativeInfinity,
            cap: Double = Double.PositiveInfinity): Double = {
    (amt min cap) max floor
  }
  
  def mean(list: Iterable[Double]): Option[Double] = {
    doIf(!list.isEmpty)(list.sum / list.size)
  }

  def meanBy[T](list: Iterable[T])(f: T => Double): Option[Double] = {
    doIf(!list.isEmpty)(list.view.map(f).sum / list.size)
  }

  def standardDeviation(list: Iterable[Double]): Option[Double] = {
    variance(list).map { variance =>
      math.sqrt(variance)
    }
  }

  def variance(list: Iterable[Double]): Option[Double] = {
    mean(list).flatMap { mean => variance(list, mean) }
  }

  def variance(list: Iterable[Double], mean: Double): Option[Double] = {
    doIf(!list.isEmpty){
      list.view.
        map(_ - mean).
        map(math.pow(_, 2)).
        sum / list.size // biaised weigthed sample variance
    }
  }
  
  def stretchIntervale(min: Double, max: Double, new_min: Double, new_max: Double)(value: Double) = {
    (value - min) / (max - min) * (new_max - new_min) + new_min
  }

  /**
   * Compute the weighted mean of a list of (weights, values)
   */
  def weighted_mean(list: Iterable[(Double, Double)]): Option[Double] = {
    val values = list.view
    val totalWeight = values.map(_._1).sum
    doIf(totalWeight > 0){
      values.map{case (w,v) => w*v}.sum / totalWeight
    }
  }

  /**
   * Compute the weighted variance of a list of (weights, values)
   * http://en.wikipedia.org/wiki/Weighted_variance#Weighted_sample_variance
   */
  def weighted_variance(list: Iterable[(Double, Double)]): Option[Double] = {
    weighted_mean(list).flatMap { weightedMean => weighted_variance(list, weightedMean) }
  }

  def weighted_variance(list: Iterable[(Double, Double)], weightedMean: Double): Option[Double] = {
    val values = list.view
    val totalWeight = values.map(_._1).sum
    doIf(totalWeight > 0){
      values.
        map{ case (w,v) => w * math.pow(v - weightedMean, 2) }.
        sum / totalWeight // biaised weigthed sample variance
    }
  }

  def pearsonCorrelationCoefficient(list: Iterable[(Option[Double], Option[Double])]): Option[Double] = {
    for {
      xMean <- mean(list.view.flatMap(_._1))
      yMean <- mean(list.view.flatMap(_._2))
      values = list.collect{ case (Some(x),Some(y)) =>
        val num = (x - xMean) * (y - yMean)
        val denumX = math.pow((x - xMean), 2)
        val denumY = math.pow((y - yMean), 2)
        (num, denumX, denumY)
      }.view
      numerator = values.map(_._1).sum
      denumX    = values.map(_._2).sum if (denumX != 0)
      denumY    = values.map(_._3).sum if (denumY != 0)
    } yield {
      numerator / math.sqrt(denumX * denumY)
    }
  }

  def spearmanCorrelationCoefficient(list: Iterable[(Double, Double)]): Option[Double] = {
    val size = list.size
    doIf(size > 1) {
      val sum = list.view.map{case (x, y) => x - y}.map(math.pow(_, 2)).sum
      1.0 - 6.0 * sum / (size * (math.pow(size, 2) - 1))
    }
  }


  /**
   * Return a list of (Option[V], Option[V]) values tuples for each key in at least one map
   */
  def unionMaps[K,V](mapLeft: Map[K, V], mapRight: Map[K, V]) = {
    (mapLeft.keySet | mapRight.keySet).toSeq map {
      key => (mapLeft.get(key), mapRight.get(key))
    }
  }

  /**
   * Return a list of (V, V) values tuples for each key in both maps
   */
  def interMaps[K,V](mapLeft: Map[K, V], mapRight: Map[K, V]) = {
    (mapLeft.keySet & mapRight.keySet).toSeq map {
      key => (mapLeft(key), mapRight(key))
    }
  }

  /**
   * Prend une liste de ratings en entrée et renvoie une map contenant pour chaque rating son rag pondéré
   * en fonction du nombre de fois qu'il apparaît dans la liste
   */
  def rankMap(list: Iterable[Double]) = {
    val unsortedRanks = list groupBy identity mapValues(_.size) // liste de couple (ratings, count)
    val ranks = SortedMap.empty[Double, Int] ++ unsortedRanks  // triée par valeur de rating

    var start = 1
    ranks.map{ case (rating, count) =>  // calcul du rang pondéré par le nombre de valeur
      val end  = start + count - 1
      val rank = (start + end) / 2.0
      start += count
      (rating -> rank)
    }
  }


  /**
   * Prend une liste de valeurs en entrée et renvoie une liste des rangs correspondants
   * Une valeur manquante (None) est remplacé par un rang valant None
   */
  def valuesToRanks(values: Iterable[(Option[Double])]) = {
    val existingValues = values.flatten 
    val ranks = rankMap(existingValues) // on récupère les rangs des valeurs présentes
    values.map{ value =>
      value.flatMap(ranks.get) // on remplace la valeur par son rang, ou None si pas de valeur
    }
  }

}
