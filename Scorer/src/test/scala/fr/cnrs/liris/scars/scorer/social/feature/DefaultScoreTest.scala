package fr.cnrs.liris.scars.scorer.social.feature

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import fr.cnrs.liris.scars.api._
import fr.cnrs.liris.scars.api.Conversion._
import fr.cnrs.liris.scars.scorer.social.impl.RecSocialScorer
import fr.cnrs.liris.scars.scorer.social.parent.SingleParent
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
class DefaultScoreTest extends SocialScorerTool {

  implicit val database = new MockDatabase()

                     database(1,3)=1.3; database(1,4)=1.4
  database(2,2)=2.2; database(2,3)=2.3; database(2,4)=2.4
  database(3,2)=3.2; database(3,3)=3.3
  database(4,2)=4.2

  val actor = new RecSocialScorer(1, 1) with SingleParent with Trust with DefaultActorMean {override protected val ProbaRating = 1.} :: Nil
  val item = new RecSocialScorer(1, 1) with SingleParent with Trust with DefaultItemMean  {override protected val ProbaRating = 1.} :: Nil
  val actorItem = new RecSocialScorer(1, 1) with SingleParent with Trust with DefaultActorMean with DefaultItemMean  {override protected val ProbaRating = 1.} :: Nil
  val itemActor = new RecSocialScorer(1, 1) with SingleParent with Trust with DefaultItemMean  with DefaultActorMean {override protected val ProbaRating = 1.} :: Nil

  test("A DefaultActorMean should return the actor mean if the rating doesn't exist") {
    implicit val scorer = actor
    score(1, 1, 1.35)
    score(1, 2, 1.35)
    score(1, 3, 1.3)
    score(1, 4, 1.4)
  }

  test("A DefaultActorMean should return no score if there is no other rating") {
    implicit val scorer = actor
    for {
      actor <- (5 to 10)
      item  <- (1 to 10)
    } score(actor, item)
  }

  
  
  test("A DefaultItemMean should return the item mean if the rating doesn't exist") {
    implicit val scorer = item
    score(1, 2, 3.2)
    score(1, 3, 1.3)
    score(1, 4, 1.4)
    
    score(3, 4, 1.9)
    
    score(4, 3, 2.3)
    score(4, 4, 1.9)
  }

  test("A DefaultItemMean should return no score if there is no other rating") {
    implicit val scorer = item
    for {
      actor <- (1 to 4)
      item  = 1
    } score(actor, item)
  }
  
  
  
  test("A DefaultActorItemMean should return the actor mean otherwise the item mean if the rating doesn't exist") {
    implicit val scorer = actorItem
    score(1, 1, 1.35)
    score(1, 2, 1.35)
    score(1, 3, 1.3)
    score(1, 4, 1.4)
    
    score(5, 2, 3.2)
    score(5, 3, 2.3)
    score(5, 4, 1.9)
  }

  test("A DefaultActorItemMean should return no score if there is no other rating") {
    implicit val scorer = actorItem
    for {
      actor <- (5 to 10)
      item  = 1
    } score(actor, item)
  }
  
  
  
  test("A DefaultItemActorMean should return the item mean otherwise the actor mean if the rating doesn't exist") {
    implicit val scorer = itemActor
    score(1, 1, 1.35)
    score(1, 2, 3.2)
    score(1, 3, 1.3)
    score(1, 4, 1.4)
    
    score(3, 1, 3.25)
    score(3, 4, 1.9)
    
    score(4, 1, 4.2)
    score(4, 3, 2.3)
    score(4, 4, 1.9)
  }

  test("A DefaultItemActorMean should return no score if there is no other rating") {
    implicit val scorer = item
    for {
      actor <- (5 to 10)
      item  = 1
    } score(actor, item)
  }
  
}
