package fr.cnrs.liris.scars.test

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import fr.cnrs.liris.scars.api._

/**
 * @author  Simon Meyffret
 * @version 0.0.1
 * @since   scala 2.8
 * Date: 9 déc. 2010
 * Time: 12:32:04
 */

trait SocialScorerTool extends FunSuite with ShouldMatchers {

  def score(actor: Actor, item: Item)(implicit scorers: List[Scorer]) = {
    scorers.foreach { scorer =>
      scorer.score(actor, item) should be (None)
    }
  }

  def score(actor: Actor, item: Item, result: Double)(implicit scorers: List[Scorer]) = {
    scorers.foreach { scorer =>
      val score = scorer.score(actor, item)
      score should be ('defined)
      score.get.value should be (result plusOrMinus 0.01)
    }
  }

}
