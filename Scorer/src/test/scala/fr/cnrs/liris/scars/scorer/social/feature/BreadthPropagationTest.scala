package fr.cnrs.liris.scars.scorer.social.feature

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

  implicit val database = new MockDatabase()
  import database.trust
  trust(1,2,1)
  trust(2,3,1)

  database(1, 1) = 1.1
  database(2, 1) = 2.1; database(2, 2) = 2.2
  database(3, 1) = 3.1; database(3, 2) = 3.2; database(3, 3) = 3.3

  implicit val _scorers = buildBreadthScorer(1, 2) :: Nil

  test("A 1-2 breadth social scorer direct friends scores first") {
    score(1, 1, 1.1)
    score(1, 2, 2.2)
    score(1, 3, 3.3)
  }
  
}
