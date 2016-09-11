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
class SimilarityActorTest extends FunSuite with ShouldMatchers {
  
  test("Positive test should check positive value") {
    val simBuilder = new SimilarityActor(positive = true)
    simBuilder.checkPositive(1) should be (true)
    simBuilder.checkPositive(0) should be (false)
    simBuilder.checkPositive(-1) should be (false)
  }

  test("Not positive test should return true always") {
    val simBuilder = new SimilarityActor(positive = false)
    simBuilder.checkPositive(1) should be (true)
    simBuilder.checkPositive(0) should be (true)
    simBuilder.checkPositive(-1) should be (true)
  }


  test("Get common raters should return the intersection") {
    implicit val db = new MockDatabase()
    db(1,1) = 1; db(1,2) = 1; db(1,3) = 1
                 db(2,2) = 1; db(2,3) = 1; db(2,4) = 1
                                           db(3,4) = 1; db(3,5) = 1

    def commonRaters(actor: Actor) = new SimilarityActor(positive = true).commonRaters(actor).toSet
    def actors(_actors: Actor*) = _actors.toSet

    commonRaters(0) should be(actors())
    commonRaters(1) should be(actors(2))
    commonRaters(2) should be(actors(1, 3))
    commonRaters(3) should be(actors(2))
  }

  {
    implicit val db = new MockDatabase()
    db(1, 1) = 0.5
    db(2, 1) = 0.1; db(2, 2) = 0.2;               db(2, 4) = 0.3  // actors 2 et 3 ont la même moyenne
    db(3, 1) = 0.8; db(3, 2) = 0.9; db(3, 3) = 1.0                // pour faciliter le calcul

    test("A small sample dataset should compute correct correlation coefficient") {
      val scores = List((0.1, 0.8), (0.2, 0.9), (0.3, 1.0)).map(t => (Some(t._1), Some(t._2)))
      val corr = Math.pearsonCorrelationCoefficient(scores)
      corr.get should be(1.0 plusOrMinus 0.001)
    }
    
    test("A small sample dataset should compute correct ratings map") {
      def ratingsMap(actor: Actor, reviews: (Actor, Item)*) = {
        val without = reviews.flatMap(db.reviewsMap.get).toSet
        new SimilarityActor(positive = true).ratingsMap(actor, without)
      }
      def map(ratings: Double*) = { // si valeur == -1, ignorée mais l'index est incrémenté
        ratings.zipWithIndex.filterNot(_._1 == -1).map{
          case (r, i) => db.getItem(i+1) -> r
        }.toMap
      }
      ratingsMap(1) should be(map(0.5))
      ratingsMap(2) should be(map(0.1, 0.2, -1, 0.3))
      ratingsMap(3) should be(map(0.8, 0.9, 1.0))
    }
    
    test("A small sample dataset should compute correct similarity") {
      def actors(token: (Actor, Actor, Double)*) = token.toList
      val seqSim = SimilarityActor.sequential(db)
      val parSim = SimilarityActor.parallele(db)
      seqSim should be equals(parSim)
      seqSim should be (actors((2, 3, 1.0)))  // rangé par actor.id
    }
  }
}
