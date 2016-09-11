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
class ConfidentSocialScorerTest extends FunSuite with ShouldMatchers {
  
  val nbUser = 10
  implicit val database = new MockDatabase()
  import database.trust
  trust(0, 1, 0.5)
  trust(0, 2, 1)
  trust(0, 3, 1)
  trust(0, 4, 0)
  trust(0, 5, 1)
  trust(1, 6, 0.9)
  trust(3, 7, 0.8)
  trust(3, 8, 0.3)
  trust(5, 9, 0.2)

  val nbItem = 1
  database(1, 1) = 0.2
  database(4, 1) = 0.8
  database(6, 1) = 0.6
  database(7, 1) = 0.9
  database(8, 1) = 0.5
  database(9, 1) = 0.1

  val scorer = buildConfidenceSocialScorer(1, 2)
  import scorer.defaultParent
  
  test("add two None ratings result a none rating") {
    scorer.merge_scores(None, None) should be (None)
  }

//  test("add some rating with confidence to None result the same rating") {
//    val r = Some(toRating(1, 0.7))
//    scorer.merge_scores(r, None) should be (r)
//    scorer.merge_scores(None, r) should be (r)
//  }
//
//  test("add some rating to some other rating with the same confidence result mean rating with same confidence") {
//    val r1 = Some(toRating(1, 0.5))
//    val r2 = Some(toRating(0, 0.5))
//    scorer.merge_scores(r1, r2) should be (Some(toRating(0.5, 0.5)))
//  }
//
//  test("add some rating to some other rating with different confidence result weighted mean rating with mean confidence") {
//    val r1 = Some(toRating(1, 0.3))
//    val r2 = Some(toRating(0, 0.7))
//    val expected = (1 * 0.3 + 0 * 0.7) / (0.3 + 0.7)
//    val confidence = (0.3 + 0.7) / 2
//    scorer.merge_scores(r1, r2) should be (Some(toRating(expected, confidence)))
//  }
//
//  test("Each user should return confident database") {
//    scorer.social_score(1, 1, defaultParent, Set(), 1, 1) should be(Some(toRating(0.2, 1)))
//    scorer.social_score(2, 1, defaultParent, Set(), 1, 1) should be(None)
//    val rating3 = scorer.social_score(3, 1, defaultParent, Set(), 1, 1).get
//    rating3.value should be(0.79 plusOrMinus 0.01)
//    rating3.confidence should be(0.55)
//    //    scorer.score(3, 1, 0, 1, 2) should be (Some(toRating(0.79, 0.5)))
//    scorer.social_score(4, 1, defaultParent, Set(), 1, 1) should be(Some(toRating(0.8, 1)))
//    val rating5 = scorer.social_score(5, 1, defaultParent, Set(), 1, 1).get
//    rating5.value should be(0.1 plusOrMinus 0.001)
//    rating5.confidence should be(0.2)
//    //    scorer.score(5, 1, 0, 1, 1) should be (Some(toRating(0.1, 0.2)))
//  }
//
//  test("The motivating example in RecSys paper should be good") {
//    scorer.score(0, 1).get should be(0.44 plusOrMinus 0.01)
//  }
}
