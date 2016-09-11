package fr.cnrs.liris.scars.scorer.social

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import fr.cnrs.liris.scars.api._
import fr.cnrs.liris.scars.api.Conversion._
import fr.cnrs.liris.scars.test.Conversion._
import fr.cnrs.liris.scars.test.SocialScorerTool

/**
 * @author  Simon Meyffret
 * @version 0.0.1
 * @since   scala 2.8
 * Date: 9 déc. 2010
 * Time: 12:32:04
 */

@RunWith(classOf[JUnitRunner])
class SocialScorerSimpleTest extends SocialScorerTool {

  val nbUser = 5
  implicit val database = new MockDatabase()
  import database.trust
  (1 to nbUser) foreach { user => ((user + 1) to nbUser) foreach {other =>
    trust(user, other, 1)
  }}

  val nbItem = 3
  database(1, 1) = 0.5
  database(2, 1) = 0.1; database(2, 2) = 0.2; database(2, 3) = 0.3
  database(3, 1) = 0.0; database(3, 2) = 1.0
  database(4, 1) = 0.5; database(4, 2) = 0.5; database(4, 3) = 0.1
  database(5, 1) = 0.1; database(5, 2) = 0.2; database(5, 3) = 0.3

  implicit val _scorers = buildCycleSocialScorer(1, 1) :: buildSocialScorer(1, 1) :: Nil

  test("A 1-1 social scorer return existing scores") {
    score(1, 1, 0.5)

    score(2, 1, 0.1)
    score(2, 2, 0.2)
    score(2, 3, 0.3)

    score(3, 1, 0.0)
    score(3, 2, 1.0)

    score(4, 1, 0.5)
    score(4, 2, 0.5)
    score(4, 3, 0.1)

    score(5, 1, 0.1)
    score(5, 2, 0.2)
    score(5, 3, 0.3)
  }

  test("A 1-1 social scorer return direct friends scores") {
    score(1, 2, (0.2 + 1.0 + 0.5 + 0.2) / 4)
    score(1, 3, (0.3 + 0.1 + 0.3) / 3)

    score(3, 3, (0.1 + 0.3) / 2)
  }
  
}
