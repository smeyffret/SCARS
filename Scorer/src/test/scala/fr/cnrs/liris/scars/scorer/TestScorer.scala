package fr.cnrs.liris.scars.scorer

import fr.cnrs.liris.scars.api._
import fr.cnrs.liris.scars.api.Conversion._

/**
 * @author  Simon Meyffret
 * @version 0.1
 * @since   scala 2.8
 * Date: 9 déc. 2010
 * Time: 12:32:04
 */

class TestScorer(value: Option[Double], var count: Int = 1) extends Scorer {
  val score = value.map(Score(_))
  def score(actor: Actor, item: Item, without: Set[Review]) = {
    if (count > 0){
      count -= 1
      score
    } else {
      throw new RuntimeException("this scorer may not be called (again)")
    }
  }
}
object ForbiddenScorer extends TestScorer(None, 0)