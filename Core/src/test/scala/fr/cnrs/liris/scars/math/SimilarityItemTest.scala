package fr.cnrs.liris.scars.math

import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import fr.cnrs.liris.scars.api._
import fr.cnrs.liris.scars.api.Conversion._

/**
 * @author  Simon Meyffret
 * @version 0.0.1
 * @since   scala 2.8
 * Date: 15/12/10
 * Time: 23:58
 */

@RunWith(classOf[JUnitRunner])
class SimilarityItemTest extends FunSuite with ShouldMatchers {


  test("Get common raters should return the intersection") {
    implicit val db = new MockDatabase()
    db(1,1) = 1; db(2,1) = 1; db(3,1) = 1
                 db(2,2) = 1; db(3,2) = 1; db(4,2) = 1
                                           db(4,3) = 1; db(5,3) = 1

    def commonRaters(item: Item) = new SimilarityItem(positive = true).commonRaters(item).toSet
    def items(_items: Item*) = _items.toSet

    commonRaters(0) should be(items())
    commonRaters(1) should be(items(2))
    commonRaters(2) should be(items(1, 3))
    commonRaters(3) should be(items(2))
  }

  {
    implicit val db = new MockDatabase()
    db(1,1) = 0.5
    db(1,2) = 0.1; db(2, 2) = 0.2;               db(4,2) = 0.3  // items 2 et 3 ont la même moyenne
    db(1,3) = 0.8; db(2,3) = 0.9; db(3, 3) = 1.0                // pour faciliter le calcul

    test("A small sample dataset should compute correct correlation coefficient") {
      val scores = List((0.1, 0.8), (0.2, 0.9), (0.3, 1.0)).map(t => (Some(t._1), Some(t._2)))
      val corr = Math.pearsonCorrelationCoefficient(scores)
      corr.get should be(1.0 plusOrMinus 0.001)
    }
    
    test("A small sample dataset should compute correct ratings map") {
      def ratingsMap(item: Item, reviews: (Actor, Item)*) = {
        val without = reviews.flatMap(db.reviewsMap.get).toSet
        new SimilarityItem(positive = true).ratingsMap(item, without)
      }
      def map(ratings: Double*) = { // si valeur == -1, ignorée mais l'index est incrémenté
        ratings.zipWithIndex.filterNot(_._1 == -1).map{
          case (r, i) => db.getActor(i+1) -> r
        }.toMap
      }
      ratingsMap(1) should be(map(0.5))
      ratingsMap(2) should be(map(0.1, 0.2, -1, 0.3))
      ratingsMap(3) should be(map(0.8, 0.9, 1.0))
    }
    
    test("A small sample dataset should compute correct similarity") {
      def items(token: (Item, Item, Double)*) = token.toList
      val seqSim = SimilarityItem.sequential(db)
      val parSim = SimilarityItem.parallele(db)
      seqSim should be equals(parSim)
      seqSim should be (items((2, 3, 1.0)))  // rangé par item.id
    }
  }
}
