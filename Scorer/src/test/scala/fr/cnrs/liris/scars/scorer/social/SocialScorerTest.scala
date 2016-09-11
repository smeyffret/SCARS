package fr.cnrs.liris.scars.scorer.social

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import fr.cnrs.liris.scars.api._
import fr.cnrs.liris.scars.api.Conversion._
import fr.cnrs.liris.scars.test.Conversion._

/**
 * @author  Simon Meyffret
 * @version 0.0.1
 * @since   scala 2.8
 * Date: 9 déc. 2010
 * Time: 12:32:04
 */

@RunWith(classOf[JUnitRunner])
class SocialScorerTest extends FunSuite with ShouldMatchers {

  val nbUser = 5
  implicit val database = new MockDatabase()
  import database.trust
  trust(1, 2, 1)                        // 1 -> 2
  trust(1, 3, 1)                        //   -> 3 -> 4
  trust(3, 4, 1)                        //        -> 5
  trust(3, 5, 1)

  val nbItem = 3
  database(2, 1) = 0.2; database(2, 2) = 0.2
  database(3, 1) = 0.3
  database(4, 1) = 0.4; database(4, 2) = 0.4; database(4, 3) = 0.4
  database(5, 1) = 0.5; database(5, 2) = 0.5; database(5, 3) = 0.5

  test("A social scorer can't have negative depth") {
    evaluating {buildSocialScorer(1, -1)} should produce[IllegalArgumentException]
  }

  test("A social scorer can't have null hops") {
    evaluating {buildSocialScorer(0, 1)} should produce[IllegalArgumentException]
  }

  val s = buildSocialScorer(1, 0)

  test("add two None ratings result a none rating") {
    s.merge_scores(None, None) should be (None)
  }

  test("add some rating to None result some rating") {
    val r1 = Some(toRating(1, count = 1))
    val r2 = Some(toRating(0, count = 3, actors = 4))
    s.merge_scores(r1, None) should be (r1)
    s.merge_scores(None, r2) should be (r2)
  }

  test("add some rating to some other rating result mean rating") {
    val r1 = Some(toRating(1, count = 1))
    val r2 = Some(toRating(0, count = 3, actors = 4))
    s.merge_scores(r1, r2) should be (Some(toRating(0.5, count = 4, actors = 5)))
  }

  test("add null trusted friends database result a None rating") {
    val ratings = Stream((toRating(1), toWeight(0)), (toRating(0), toWeight(0))).map(Some(_))
    s.friends_score(ratings) should be (None)
  }

  test("add trusted friends database result some rating") {
    val ratings = Stream((toRating(1), toWeight(1)), (toRating(0), toWeight(1))).map(Some(_))
    s.friends_score(ratings) should be (Some(toRating(0.5, count = 2, actors = 2)))
  }

  test("add trusted friends database with some confident ratings result some other confident rating using ConfidentSocialScorer") {
    val t = buildConfidenceSocialScorer(1, 1)
    val friends_scores = Stream((toRating(1, 0), toWeight(1)), (toRating(0, 1), toWeight(1))).map(Some(_))
    t.friends_score(friends_scores) should be (Some(toRating(0.5, 0.5, count = 2, actors = 2)))
  }
}