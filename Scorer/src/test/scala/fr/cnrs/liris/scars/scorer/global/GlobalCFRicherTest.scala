package fr.cnrs.liris.scars.scorer.global

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import fr.cnrs.liris.scars.api._
import fr.cnrs.liris.scars.api.Conversion._
import fr.cnrs.liris.scars.test.Conversion._
import fr.cnrs.liris.scars.math._

/**
 * @author  Simon Meyffret
 * @version 0.0.1
 * @since   scala 2.8
 * Date: 9 déc. 2010
 * Time: 12:32:04
 */

@RunWith(classOf[JUnitRunner])
class GlobalCFRicherTest extends FunSuite with ShouldMatchers {

  implicit val db = new MockDatabase()

  db(1, 1) = 1; db(1, 2) = 2; db(1, 3) = 3; db(1, 4) = 4; 
                db(2, 2) = 3; db(2, 3) = 4; db(2, 4) = 5; db(2,5)=2
  db(3, 1) = 3;               db(3, 3) = 5; db(3, 4) = 6;          db(3,6)=4
  db(4, 1) = 4; db(4, 2) = 5;               db(4, 4) = 7;                   db(4,7)=6
  db(5, 1) = 5; db(5, 2) = 6; db(5, 3) = 7;                                          db(5,8)=8
  db(6, 1) = 6; db(6, 2) = 7; db(6, 3) = 8; db(6, 4) = 9;
              

  val scorer = new GlobalCF(false)
  val cache_scorer = new GlobalCF()
  def computeSim(db: Database) = SimilarityActor.sequential(db).foreach { case (actor, other, similarity) => 
    actor.asInstanceOf[MockActor].addSimilarity(other, similarity)
    other.asInstanceOf[MockActor].addSimilarity(actor, similarity)
  }
  computeSim(db)

  {
    def similarity(actor: Actor, other: Actor, sim: Double = 1.0) = {
      actor.similarity(other).get should be(sim plusOrMinus 0.00001)
    }
    def similars(actor: Actor, others: Actor*) = {
      test("%s is similar to %s".format(actor, others)) {
        actor.similars should have size (others.size)
        others.foreach(similarity(actor, _))
      }
    }
    similars(1, 2, 3, 4, 5, 6)
    similars(2, 1, 3, 4, 5, 6)
    similars(3, 1, 2, 4, 5, 6)
    similars(4, 1, 2, 3, 5, 6)
    similars(5, 1, 2, 3, 4, 6)
    similars(6, 1, 2, 3, 4, 5)
  }
  
  {
    scores(1)(      3, 4, 5, 6)(   3,    5, 6, 7)(   4, 5,    7, 8)(   5, 6, 7,    9)
    scores(2)(1,    3, 4, 5, 6)(2,             7)(3,             8)(4,             9)
    scores(3)(1,             6)(2, 3,    5, 6, 7)(3,             8)(4,             9)
    scores(4)(1,             6)(2,             7)(3, 4, 5,    7, 8)(4,             9)
    scores(5)(1,             6)(2,             7)(3,             8)(4, 5, 6, 7,    9)
    scores(6)(1,    3, 4, 5   )(2, 3,    5, 6   )(3, 4, 5,    7   )(4, 5, 6, 7      )
  }

  def scores(actor: Actor)(vals1: Double*)(vals2: Double*)(vals3: Double*)(vals4: Double*) = {
    scoreItem(actor, 1, vals1: _*)
    scoreItem(actor, 2, vals2: _*)
    scoreItem(actor, 3, vals3: _*)
    scoreItem(actor, 4, vals4: _*)
  }
  
  def scoreItem(actor: Actor, item: Item, values: Double*) = {
    if (values.isEmpty)
      score(actor, item, None)
    else
      score(actor, item, Some(values.reduceLeft(_+_) / values.size))  
  }
  
  def score(actor: Actor, item: Item, result: Option[Double]) = {
    val without = actor.review(item).toSet
    test("%s's score for %s without %s should be %s".format(actor, item, without, result)) {
//      scorer.score(actor, item, without) should be (result)
    }
    test("%s's score for %s without %s should be %s (using cache)".format(actor, item, without, result)) {
//      cache_scorer.score(actor, item, without) should be (result)
    }
  }
  
}
