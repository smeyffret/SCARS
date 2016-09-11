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
class SocialScorerHopsTest extends SocialScorerTool {

  implicit val database = new MockDatabase()
  import database.trust
  trust(1, 2, 1)                        // 1 -> 2
  trust(1, 3, 1)                        //   -> 3 -> 4
  trust(3, 4, 1)                        //        -> 5
  trust(3, 5, 1)

  database(2, 1) = 0.2; database(2, 2) = 0.2
  database(3, 1) = 0.3
  database(4, 1) = 0.4; database(4, 2) = 0.4; database(4, 3) = 0.4
  database(5, 1) = 0.5; database(5, 2) = 0.5; database(5, 3) = 0.5

  test("A 1-1 social scorer return existing 1-1 scores") {
    implicit val _scorers = buildCycleSocialScorer(1, 1) :: buildSocialScorer(1, 1) :: Nil
    score(1, 1, (0.2 + 0.3) / 2)
    score(1, 2, 0.2)
    score(1, 3)
  }

  test("A 1-2 social scorer return existing 1-2 scores") {
    implicit val _scorers = buildCycleSocialScorer(1, 2) :: buildSocialScorer(1, 2) :: Nil
    score(1, 1, (0.2 + 0.3) / 2)
    val oneHop3 = (0.4 + 0.5) / 2
    score(1, 2, (0.2 + oneHop3) / 2)
    score(1, 3, oneHop3)
  }

  test("A 2-2 social scorer return existing 2-2 scores") {
//    implicit val _scorers = buildSocialScorer(2, 2) :: buildCycleSocialScorer(2, 2) :: Nil // pas encore implémentée
    implicit val _scorers = buildSocialScorer(2, 2) :: Nil

    val score21 = 0.2
    val oneHop31 = (0.4 + 0.5) / 2
    val score31 = (0.3 + oneHop31) / 2
    score(1, 1, (score21 + score31) / 2)

    val score22 = 0.2
    val oneHop32 = (0.4 + 0.5) / 2
    val score32 = oneHop32
    score(1, 2, (score22 + score32) / 2)

    score(1, 3, (0.4 + 0.5) / 2)
  }

}
