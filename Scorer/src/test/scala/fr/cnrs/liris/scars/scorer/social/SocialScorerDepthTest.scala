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
class SocialScorerDepthTest extends SocialScorerTool {

  implicit val database = new MockDatabase()
  import database.trust
  trust(1, 2, 1)                        // 1 -> 2
  trust(1, 3, 1)                        //   -> 3 -> 4 ---v
  trust(3, 4, 1)                        //        -> 5 -> 6
  trust(3, 5, 1)                        //           '--> 7 -> 8
  trust(4, 6, 1)
  trust(5, 6, 1)
  trust(5, 7, 1)
  trust(6, 7, 1)
  trust(7, 8, 1)

  database(4, 1) = 0.4;                                                                                       
                        database(5, 2) = 0.5;                                                                 
  database(6, 1) = 0.6; database(6, 2) = 0.6; database(6, 3) = 0.6; database(6, 4) = 0.6;                     
  database(7, 1) = 0.7; database(7, 2) = 0.7; database(7, 3) = 0.7;                       database(7, 5) = 0.7
  database(8, 1) = 0.8; database(8, 2) = 0.8; database(8, 3) = 0.8;                       database(8, 5) = 0.8; database(8, 6) = 0.8

  test("A 1-3 social scorer return existing 1-3 scores") {
    implicit val _scorers = buildCycleSocialScorer(1, 3) :: buildSocialScorer(1, 3) :: Nil
    score(1, 1, (0.4 + (0.6 + 0.7) / 2) / 2)
    score(1, 2, (0.6 + 0.5) / 2)
    score(1, 3, (0.6 + (0.6 + 0.7) / 2) / 2)
    score(1, 4, (0.6 + 0.6) / 2)
    score(1, 5, 0.7)
    score(1, 6)
  }

  test("A 1-4 social scorer return existing 1-3 scores") {
    implicit val _scorers = buildCycleSocialScorer(1, 4) :: buildSocialScorer(1, 4) :: Nil
    score(1, 1, (0.4 + (0.6 + 0.7) / 2) / 2)
    score(1, 2, (0.6 + 0.5) / 2)
    score(1, 3, (0.6 + (0.6 + 0.7) / 2) / 2)
    score(1, 4, (0.6 + 0.6) / 2)
    score(1, 5, 0.7)
    
    score(1, 6, 0.8)
  }

  test("A 1-5 social scorer return existing 1-3 scores") {
    implicit val _scorers = buildCycleSocialScorer(1, 5) :: buildSocialScorer(1, 5) :: Nil
    score(1, 1, (0.4 + (0.6 + 0.7) / 2) / 2)
    score(1, 2, (0.6 + 0.5) / 2)
    score(1, 3, (0.6 + (0.6 + 0.7) / 2) / 2)
    score(1, 4, (0.6 + 0.6) / 2)
    score(1, 5, 0.7)
    
    score(1, 6, 0.8)
  }

}
