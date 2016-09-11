package fr.cnrs.liris.scars.file.epinions

import org.scalatest.junit.JUnitRunner
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
class FileDatasetSimpleTest extends FunSuite with ShouldMatchers with FileDatasetCondition {
  
  test("Parsing nothing should return nothing") {
    val dataset = FileDataset(
        empty_ratings_source
    )
    
    emptynessCondition(dataset)
    val database = dataset.build()
    emptynessCondition(dataset)
    emptynessCondition(database)
    
  }

  test("Parsing empty sources should return nothing") {
    val empty = Some(empty_ratings_source)
    val dataset = FileDataset(
        empty_ratings_source,
        empty, empty, empty, empty, empty, empty
    )
    
    emptynessCondition(dataset)
    val database = dataset.build()
    emptynessCondition(dataset)
    emptynessCondition(database)
    
  }

  test("Parsing categories should return all categories") {
    val categories_list = List(1, 2, 3)
    val categories_source = Some(fromString(categories_list.mkString("\n")))
    val dataset = FileDataset(
        empty_ratings_source,
        categories_source = categories_source
    )
        
    val database = dataset.build()
    emptynessCondition(dataset, dataset.categories)
    val categories = dataset.categories.mapValues(c => (c.id, c.name, c.parent))
    
    categories should have size (categories_list.size)
    val result = categories_list.map(id => id -> (id, "Category" + id, None)).toMap
	categories should be === (result)
	
  }

  test("Parsing actors should return all actors with categories expertness") {
    val actors_list = List("1 1 2", "2", "3 3", "4 1 2 3 4")
    val actors_source = Some(fromString(actors_list.mkString("\n")))
    
    val dataset = FileDataset(
        empty_ratings_source,
        actors_source = actors_source
    )
        
    val database = dataset.build()
    emptynessCondition(dataset, dataset.actors, dataset.categories)
    
    dataset.categories.keys should be === (Set(1,2,3,4))
    dataset.actors.keys should be === (Set(1,2,3,4))
    
    def actorMatch(id_actor: Int, id_categories: Int*) = {
      dataset.actors(id_actor).expertises.map(_.id) should be === (id_categories.toSet)
    }
    actorMatch(1, 1, 2)
    actorMatch(2)
    actorMatch(3, 3)
    actorMatch(4, 1, 2, 3, 4)
	
  }

  test("Parsing items should return all items with associated category") {
    val items_list = List("1 1", "2 1", "3 2")
    val items_source = Some(fromString(items_list.mkString("\n")))
    
    val dataset = FileDataset(
        empty_ratings_source,
        items_source = items_source
    )
        
    val database = dataset.build()
    emptynessCondition(dataset, dataset.items, dataset.categories)
    
    dataset.categories.keys should be === (Set(1,2))
    dataset.items.keys should be === (Set(1,2,3))
    
    def itemMatch(id_item: Int, id_category: Int) = {
      dataset.items(id_item).category.id should be (id_category)
    }
    itemMatch(1, 1)
    itemMatch(2, 1)
    itemMatch(3, 2)
	
  }

  test("Parsing trust should return all actors with associated trust") {
    val trust_list = List("1 1 1", "2 1 0.5", "3 2 0", "2\t4")
    val trust_source = Some(fromString(trust_list.mkString("\n")))
    
    val dataset = FileDataset(
        empty_ratings_source,
        trust_source = trust_source
    )
        
    val database = dataset.build()
    emptynessCondition(dataset, dataset.actors)
    
    dataset.actors.keys should be === (Set(1,2,3,4))
    
    def trustMatch(id_actor: Int, id_friend: Int, trust: Double) = {
      val actor = dataset.actors(id_actor)
      val friend = dataset.actors(id_friend)
      actor.trust(friend) should be (Some(trust))
      friend.followers.contains(actor) should be (true)
    }
    trustMatch(1, 1, 1)
    trustMatch(2, 1, 0.5)
    trustMatch(3, 2, 0)
	
    trustMatch(2, 4, 1)
    trustMatch(4, 2, 1)
  }

  test("Parsing similarity should return all actors with associated similarity") {
    val similarity_list = List("1 1 1", "2 1 0.5", "3 2 0")
    val similarity_source = Some(fromString(similarity_list.mkString("\n")))
    
    val dataset = FileDataset(
        empty_ratings_source,
        similarity_source = similarity_source
    )
        
    val database = dataset.build()
    emptynessCondition(dataset, dataset.actors)
    
    dataset.actors.keys should be === (Set(1,2,3))
    
    def similarityMatch(id_actor: Int, id_similar: Int, similarity: Double) = {
      val actor = dataset.actors(id_actor)
      val similar = dataset.actors(id_similar)
      actor.similarity(similar) should be (Some(similarity))
      similar.followers.contains(actor) should be (false)
    }
    similarityMatch(1, 1, 1)
    similarityMatch(2, 1, 0.5)
    similarityMatch(1, 2, 0.5)
    similarityMatch(3, 2, 0)
    similarityMatch(2, 3, 0)
	
  }
}
