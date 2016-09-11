package fr.cnrs.liris.scars.scorer.social

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import fr.cnrs.liris.scars.api._
import fr.cnrs.liris.scars.api.Conversion._
import fr.cnrs.liris.scars.test.Conversion._
import fr.cnrs.liris.scars.scorer.util._
import fr.cnrs.liris.scars.scorer.Builder.DeltaSocialScorer
import fr.cnrs.liris.scars.test.SocialScorerTool

/**
 * @author  Simon Meyffret
 * @version 0.0.1
 * @since   scala 2.8
 * Date: 9 déc. 2010
 * Time: 12:32:04
 */

@RunWith(classOf[JUnitRunner])
class DeltaSocialScorerSimpleTest extends SocialScorerTool {

  implicit val settings = DeltaSettings(0, 1, None)
  implicit val database = new MockDatabase()
  import database.trust
  trust(1, 2, 1)                        // 1 -> 2
  trust(1, 3, 1)                        //   -> 3 -> 4

  database(2, 1) = 0.2; database(2, 2) = 0.2
  database(3, 1) = 0.3

  implicit val scorers = DeltaSocialScorer(1, 1) :: buildDeltaCycleSocialScorer(1, 1) :: Nil

  test("A user without ratings doesn't score with a delta scorer without default mean") {
    score(1, 1)
    score(1, 2)
    score(1, 3)
  }


  test("A user with one rating without default mean scores with a delta scorer, except without his rating") {
    database(1, 42) = 0.8; database(1, 43) = 0.8
    score(1, 1, 0.8)
    score(1, 2, 0.8)
    score(1, 3)
  }

}
