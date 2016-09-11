package fr.cnrs.liris.scars.scorer.util

import fr.cnrs.liris.scars.api._
import java.io.File

/**
 * Read scores in a matrix, does not compute them
 *
 * @author  Simon Meyffret
 * @version 0.0.1
 * @since   scala 2.8
 * Date: 21/12/10
 * Time: 21:39
 */

object RatingsScorer {
  
  def apply(file: File) = {
    val path = file.getAbsolutePath
    val ratings = buildRatings(path)
    new RatingsScorer(ratings, file.getName)
  }
  
  private def buildRatings(path: String): Map[(Int, Int), Score] = {
    io.Source.fromFile(path).getLines.map(_.trim.split(" ").toList).collect{ 
      case actor :: item :: rating :: Nil =>
        (actor.toInt, item.toInt) -> Score(rating.toDouble)
      case actor :: item :: rating :: confidence :: Nil =>
        (actor.toInt, item.toInt) -> Score(rating.toDouble, confidence.toDouble)
      case actor :: item :: rating :: confidence :: count :: Nil =>
        (actor.toInt, item.toInt) -> Score(rating.toDouble, confidence.toDouble, count.toInt)
    }.toMap
  }
}

class RatingsScorer(ratings: Map[(Int, Int), Score], description: String = "ratings") extends Scorer {

  override def score(actor: Actor, item: Item, without: Set[Review]) = {
    ratings get (actor.id, item.id)
  }

  override def toString = description

}