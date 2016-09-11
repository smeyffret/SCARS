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
class MockDatabaseTest extends FunSuite with ShouldMatchers {

  implicit val database = new MockDatabase()
  database(1,1) = 1
  database(1,2) = 1
  database(1,3) = 1
  database(2,2) = 1
  database(2,3) = 1
  database(2,4) = 1
  database(3,4) = 1
  database(3,5) = 1

  test("Insert ratings should repercut on actors") {
    actorItems(1) should be (items(1,2,3))
    actorItems(2) should be (items(2,3,4))
    actorItems(3) should be (items(4,5))
  }
  
  test("Insert ratings should repercut on items") {
    itemActors(1) should be (actors(1))
    itemActors(2) should be (actors(1,2))
    itemActors(3) should be (actors(1,2))
    itemActors(4) should be (actors(2,3))
    itemActors(5) should be (actors(3))
  }
  
  def itemActors(id: Int) = {
    database.getItem(id).reviews.map(_.actor).toSet
  }
  
  def actorItems(id: Int) = {
    database.getActor(id).reviews.map(_.item).toSet
  }
  
  def items(ids: Int*) = {
    ids.toSet.map(database.getItem)
  }

  def actors(ids: Int*) = {
    ids.toSet.map(database.getActor)
  }

}