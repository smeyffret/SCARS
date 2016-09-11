package fr.cnrs.liris.scars.file.epinions

import org.scalatest.junit.JUnitRunner
import java.util.Date
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import fr.cnrs.liris.scars.api.Conversion._
import fr.cnrs.liris.scars.api._
import fr.cnrs.liris.scars.api.impl._
import io.Source.fromString

/**
 * @author  Simon Meyffret
 * @version 0.0.1
 * @since   scala 2.8
 * Date: 9 déc. 2010
 * Time: 13:06:02
 */

@RunWith(classOf[JUnitRunner])
class FileDatasetReviewTest extends FunSuite with ShouldMatchers with FileDatasetCondition {

  def review(id_actor: Int, id_item: Int, rating: Double, confidence: Option[Double] = None, date: Option[Date] = None)(implicit dataset: FileDataset) = {
    val actor = dataset.actors(id_actor)
    val item = dataset.items(id_item)
    val review = actor.review(item).get
    review.rating should be(rating)
    confidence.foreach{ c => review.confidence should be(c) }
    date.foreach{ _ => review.date should be(date) }
    item.reviews.contains(review) should be(true)
  }

  test("Parsing reviews should return corresponding actors, items and reviews (+ confidence and data)") {
    val reviews_list = List("1 1 1", "2 3 5 0.5", "3 7 2.6 2000-01-01", "4 9 3 0.3 2000-01-01")
    val reviews_source = fromString(reviews_list.mkString("\n"))
    implicit val dataset = FileDataset(
      reviews_source,
      Some(empty_ratings_source) // no evalReviews
      )

    val database = dataset.build()
    emptynessCondition(dataset, dataset.actors, dataset.items)
    emptynessCondition(database, database.actors, database.items, database.reviews)

    dataset.actors.keys should be === (Set(1, 2, 3, 4))
    dataset.items.keys should be === (Set(1, 3, 7, 9))
    database.reviews should have size (reviews_list.size)

    val date = new java.text.SimpleDateFormat("dd-MM-yyyy").parse("01-01-2000")
    review(1, 1, 1)
    review(2, 3, 5, confidence = Some(0.5))
    review(3, 7, 2.6, date = Some(date))
    review(4, 9, 3, confidence = Some(0.3), date = Some(date))

  }

  test("Parsing evalReviews should return corresponding actors, items (without review) and evalReviews") {
    val reviews_list = List("1 1 1", "2 3 5", "3 7 2.6")
    val reviews_source = fromString(reviews_list.mkString("\n"))
    implicit val dataset = FileDataset(
      empty_ratings_source,
      Some(reviews_source))

    val database = dataset.build()
    emptynessCondition(dataset, dataset.actors, dataset.items)
    emptynessCondition(database, database.actors, database.items, database.evalReviews)

    dataset.actors.keys should be === (Set(1, 2, 3))
    dataset.items.keys should be === (Set(1, 3, 7))
    database.evalReviews should have size (reviews_list.size)

    val evalReviews = database.evalReviews.map(review => (review.actor.id, review.item.id, review.rating))

    evalReviews should be === (List((1, 1, 1), (2, 3, 5), (3, 7, 2.6)))

    database.actors.foreach { actor => actor.reviews should be('empty) }
    database.items.foreach { item => item.reviews should be('empty) }

  }

  test("Parsing same reviews and evalReviews should return corresponding actors, items with review and evalReviews") {
    val reviews_list = List("1 1 1", "2 3 5", "3 7 2.6")
    val reviews_source = fromString(reviews_list.mkString("\n"))
    implicit val dataset = FileDataset(
      reviews_source,
      Some(reviews_source))

    val database = dataset.build()
    emptynessCondition(dataset, dataset.actors, dataset.items)
    emptynessCondition(database, database.actors, database.items, database.reviews, database.evalReviews)

    dataset.actors.keys should be === (Set(1, 2, 3))
    dataset.items.keys should be === (Set(1, 3, 7))
    database.reviews should have size (reviews_list.size)
    database.evalReviews should have size (reviews_list.size)

    val evalReviews = database.evalReviews.map(review => (review.actor.id, review.item.id, review.rating))

    evalReviews should be === (List((1, 1, 1), (2, 3, 5), (3, 7, 2.6)))

    review(1, 1, 1)
    review(2, 3, 5)
    review(3, 7, 2.6)

  }

  test("Parsing differents reviews and evalReviews should return corresponding actors, items with review and evalReviews") {
    val reviews_list = List("1 1 1", "2 3 5", "3 7 2.6", "3 3 0")
    val eval_reviews_list = List("1 3 1", "2 8 5")
    val reviews_source = fromString(reviews_list.mkString("\n"))
    val eval_reviews_source = fromString(eval_reviews_list.mkString("\n"))
    implicit val dataset = FileDataset(
      reviews_source,
      Some(eval_reviews_source))

    val database = dataset.build()
    emptynessCondition(dataset, dataset.actors, dataset.items)
    emptynessCondition(database, database.actors, database.items, database.reviews, database.evalReviews)

    dataset.actors.keys should be === (Set(1, 2, 3))
    dataset.items.keys should be === (Set(1, 3, 7, 8))
    database.reviews should have size (reviews_list.size)
    database.evalReviews should have size (eval_reviews_list.size)

    val evalReviews = database.evalReviews.map(review => (review.actor.id, review.item.id, review.rating))

    evalReviews should be === (List((1, 3, 1), (2, 8, 5)))

    review(1, 1, 1)
    review(2, 3, 5)
    review(3, 7, 2.6)
    review(3, 3, 0)

  }

}