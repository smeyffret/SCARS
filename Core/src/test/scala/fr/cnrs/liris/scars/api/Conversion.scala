package fr.cnrs.liris.scars.api

import scala.runtime.RichInt

/**
 * @author  Simon Meyffret
 * @version 0.0.1
 * @since   scala 2.8
 * Date: 31 janv. 2011
 * Time: 16:55:32
 */

object Conversion {
  
  implicit def intToActor(id: Int)(implicit database: MockDatabase) = database.getActor(id)
  implicit def intToItem(id: Int)(implicit database: MockDatabase) = database.getItem(id)

  def toActor(id: Int) = new MockActor(id)
  def toItem(id: Int) = new MockItem(id)
  implicit def toWeight(value: Double) = value
  implicit def toWeight(value: RichInt): Double = toWeight(value.toDouble) // pour éviter un conflit entre int -> RichInt et int -> Trust

  type Scores = List[(Actor, Item, Double, Option[Double])]

  def toScores(list: List[(Actor, Item, Double, Option[Double])]) = {
    list.collect {
      case (actor, item, rating, Some(score)) => (actor, item, rating, score)
    }
  }
  
  implicit def toDouble(score: fr.cnrs.liris.scars.api.Score) = score.value
  implicit def toScore(value: Double) = fr.cnrs.liris.scars.api.Score(value)

}
