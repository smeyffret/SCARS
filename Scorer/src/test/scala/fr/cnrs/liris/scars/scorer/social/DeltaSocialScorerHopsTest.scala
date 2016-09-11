package fr.cnrs.liris.scars.scorer.social

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import fr.cnrs.liris.scars.api._
import fr.cnrs.liris.scars.api.Conversion._
import fr.cnrs.liris.scars.test.Conversion._
import fr.cnrs.liris.scars.scorer.util._
import fr.cnrs.liris.scars.scorer.Builder.{DeltaSocialScorer, defaultSettings}
import fr.cnrs.liris.scars.test.SocialScorerTool

/**
 * @author  Simon Meyffret
 * @version 0.0.1
 * @since   scala 2.8
 * Date: 9 déc. 2010
 * Time: 12:32:04
 */

@RunWith(classOf[JUnitRunner])
class DeltaSocialScorerHopsTest extends SocialScorerTool {

  implicit val database = new MockDatabase()
  import database.trust
  trust(1, 2, 1)                        // 1 -> 2
  trust(1, 3, 1)                        //   -> 3 -> 4
  trust(3, 4, 1)                        //        -> 5
  trust(3, 5, 1)

  database(1, 42) = 0.8                              // moyenne 0.8 - default 0.5
  database(2, 1) = 0.1; database(2, 2) = 0.2                // moyenne 0.15
  database(3, 1) = 0.3                               // moyenne 0.3 - default 0.5
  database(4, 1) = 0.1; database(4, 2) = 0.1; database(4, 3) = 0.4 // moyenne 0.2
  database(5, 1) = 0.2; database(5, 2) = 0.7; database(5, 3) = 0.9 // moyenne 0.6

  test("A 1-1 delta social scorer return existing 1-1 scores") {
    implicit val scorers = DeltaSocialScorer(1, 1, minReviews = 2) :: buildDeltaCycleSocialScorer(1, 1, minReviews = 2) :: Nil
    score(1, 1, 0.5 + (-0.05 - 0.2) / 2)
    score(1, 2, 0.5 + 0.05)
    score(1, 3)
  }

  test("A 1-2 delta social scorer return existing 1-2 scores") {
    implicit val scorers = DeltaSocialScorer(1, 2, minReviews = 2) :: buildDeltaCycleSocialScorer(1, 2, minReviews = 2) :: Nil
    score(1, 1, 0.5 + (-0.05 - 0.2) / 2)
    val oneHop32 = (-0.1 + 0.1) / 2
    score(1, 2, 0.5 + (0.05 + oneHop32) / 2)
    val oneHop33 = (0.2 + 0.3) / 2
    score(1, 3, 0.5 + oneHop33)
  }

  test("A 2-2 delta social scorer return existing 2-2 scores") {
//    implicit val scorers = DeltaSocialScorer(2, 2, minReviews = 2) :: buildDeltaCycleSocialScorer(2, 2, minReviews = 2) :: Nil
    implicit val scorers = DeltaSocialScorer(2, 2, minReviews = 2) :: Nil

    val score21 = -0.05
    val oneHop31 = (-0.1 - 0.4) / 2
    val score31 = (-0.2 + oneHop31) / 2
    score(1, 1, 0.5 + (score21 + score31) / 2)

    val score22 = 0.05
    val oneHop32 = (-0.1 + 0.1) / 2
    val score32 = oneHop32
    score(1, 2, 0.5 + (score22 + score32) / 2)

    score(1, 3, 0.5 + (0.2 + 0.3) / 2)
  }

}
