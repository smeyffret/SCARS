package fr.cnrs.liris.scars.database.epinions

import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import fr.cnrs.liris.scars.api._
import fr.cnrs.liris.scars.api.impl._
import fr.cnrs.liris.scars.database.epinions.database.MockQueryDatabase

/**
 * @author  Simon Meyffret
 * @version 0.0.1
 * @since   scala 2.8
 * Date: 9 déc. 2010
 * Time: 13:06:02
 */

@RunWith(classOf[JUnitRunner])
class DBDatasetSimpleTest extends FunSuite with ShouldMatchers with DBDatasetCondition {
  
  test("Parsing nothing should return nothing") {
    val dataset = DBDataset(
        new MockQueryDatabase()
    )
    
    emptynessCondition(dataset)
    val database = dataset.build()
    emptynessCondition(dataset)
    emptynessCondition(database)
    
  }

  test("Parsing categories should return all categories") {
    val categories_data = (1 to 3).map(id => (id, "Category" + id, 0))
    val dataset = DBDataset(
        new MockQueryDatabase(all_categories = categories_data)
    )
        
    val database = dataset.build()
    emptynessCondition(dataset, dataset.categories)
    val categories = dataset.categories.mapValues(c => (c.id, c.name, c.parent))
    
    categories should have size (categories_data.size)
    val result = (1 to 3).map(id => id -> (id, "Category" + id, None)).toMap
	categories should be === (result)
	
  }

  test("Parsing categories with parent should return all categories and children") {
    val categories_data = List( (1, 0), (2, 1), (3, 1) ).map{
      case (id, parent) => (id, "Category" + id, parent)
      }
    val dataset = DBDataset(
        new MockQueryDatabase(all_categories = categories_data)
    )
        
    val database = dataset.build()
    emptynessCondition(dataset, dataset.categories)
    val categories = dataset.categories.mapValues(c => (c.id, c.parent.map(_.id)))
    
    categories should have size (categories_data.size)
    val result = Set(1 -> None, 2 -> Some(1), 3 -> Some(1))
	categories.values.toSet should be === (result)
	
  }

  test("Parsing actors should return all actors") {
    val actors_data = (1 to 3).map(id => (id, id))
    
    val dataset = DBDataset(
        new MockQueryDatabase(all_actors = actors_data)
    )
        
    val database = dataset.build()
    emptynessCondition(dataset, dataset.actors)
    
    val actors = dataset.actors.mapValues(a => (a.id, Some(a.id)))
    
    actors should have size (actors_data.size)
    val result = (1 to 3).map(id => id -> (id, Some(id))).toMap
	actors should be === (result)
	
  }
  

  test("Parsing actors and categories should return all actors with categories expertness") {
    val categories_data = (1 to 3).map(id => (id, "Category" + id, 0))
    val actors_data = (1 to 3).map(id => (id, id))
    val tops_data = actors_data.zip(categories_data).map{
      case ((idactor, _), (idcategory, _, _)) => (idactor, idcategory, Expertise.Leader.toString)
    }
    
    val dataset = DBDataset(
        new MockQueryDatabase(all_actors = actors_data, all_categories = categories_data, all_experts = tops_data)
    )
        
    val database = dataset.build()
    emptynessCondition(dataset, dataset.actors, dataset.categories)
    dataset.actors.keys should be === (Set(1,2,3))
    dataset.categories.keys should be === (Set(1,2,3))
    
    val actors = dataset.actors.mapValues(c => (c.id, c.expertises.size, c.expertises.head.id))
    val categories = dataset.categories.mapValues(c => (c.id, c.experts.size, c.experts.head.id))
    
    actors should have size (actors_data.size)
    val result = (1 to 3).map(id => id -> (id, 1, id)).toMap
    actors should be === (result)
    categories should be === (result)
	
  }

  test("Parsing actors without categories should return all actors with categories expertness") {
    val categories_data = (1 to 3).map(id => (id, "Category" + id, 0))
    val actors_data = (1 to 3).map(id => (id, 0))
    val tops_data = actors_data.zip(categories_data).map{
      case ((idactor, _), (idcategory, _, _)) => (idactor, idcategory, Expertise.Leader.toString)
    }
    
    val dataset = DBDataset(
        new MockQueryDatabase(all_actors = actors_data, all_experts = tops_data)
    )
        
    val database = dataset.build()
    emptynessCondition(dataset, dataset.actors, dataset.categories)
    dataset.actors.keys should be === (Set(1,2,3))
    dataset.categories.keys should be === (Set(1,2,3))
    
    val actors = dataset.actors.mapValues(c => (c.id, c.expertises.size, c.expertises.head.id))
    
    actors should have size (actors_data.size)
    val result = (1 to 3).map(id => id -> (id, 1, id)).toMap
	actors should be === (result)
	
  }

  test("Parsing items should return all items") {
    val items_data = (1 to 3).map(id => (id, "Product" + id, 1))
    
    val dataset = DBDataset(
        new MockQueryDatabase(all_items = items_data)
    )
        
    val database = dataset.build()
    emptynessCondition(dataset, dataset.items, dataset.categories)
    
    dataset.items.keys should be === (Set(1,2,3))
    dataset.categories.keys should be === (Set(1))
    
  }

  test("Parsing items and categories should return all items with associated category") {
    val categories_data = (1 to 3).map(id => (id, "Category" + id, 0))
    val items_data = (1 to 3).map(id => (id, "Product" + id, id))
    
    val dataset = DBDataset(
        new MockQueryDatabase(all_items = items_data, all_categories = categories_data)
    )
        
    val database = dataset.build()
    emptynessCondition(dataset, dataset.items, dataset.categories)
    
    dataset.categories.keys should be === (Set(1,2,3))
    dataset.items.keys should be === (Set(1,2,3))
    
    def itemMatch(id_item: Int, id_category: Int) = {
      dataset.items(id_item).category.id should be (id_category)
    }
    itemMatch(1, 1)
    itemMatch(2, 2)
    itemMatch(3, 3)
	
  }

  test("Parsing items without categories should return all items with associated category") {
    val categories_data = (1 to 3).map(id => (id, "Category" + id, 0))
    val items_data = (1 to 3).map(id => (id, "Product" + id, id))
    
    val dataset = DBDataset(
        new MockQueryDatabase(all_items = items_data)
    )
        
    val database = dataset.build()
    emptynessCondition(dataset, dataset.items, dataset.categories)
    
    dataset.categories.keys should be === (Set(1,2,3))
    dataset.items.keys should be === (Set(1,2,3))
    
    def itemMatch(id_item: Int, id_category: Int) = {
      dataset.items(id_item).category.id should be (id_category)
    }
    itemMatch(1, 1)
    itemMatch(2, 2)
    itemMatch(3, 3)
	
  }

  test("Parsing trust should return all actors with associated trust") {
    val actors_data = (1 to 3).map(id => (id, 0))
    val trusts_data = List( (1, 2, 1.), (1, 3, 1.), (2, 3, 1.) )
    
    val dataset = DBDataset(
        new MockQueryDatabase(all_actors = actors_data, all_trusts = trusts_data)
    )
        
    val database = dataset.build()
    emptynessCondition(dataset, dataset.actors)
    
    dataset.actors.keys should be === (Set(1,2,3))
    
    def trustMatch(id_actor: Int, id_friend: Int, trust: Double) = {
      val actor = dataset.actors(id_actor)
      val friend = dataset.actors(id_friend)
      actor.trust(friend) should be (Some(trust))
      friend.followers.contains(actor) should be (true)
    }
    trustMatch(1, 2, 1)
    trustMatch(1, 3, 1)
    trustMatch(2, 3, 1)
	
  }

  test("Parsing similarities should return all actors with associated similarity") {
    val actors_data = (1 to 3).map(id => (id, 0))
    val similarities_data = List( (1, 2, 1.), (1, 3, 1.), (2, 3, 1.) )
    
    val dataset = DBDataset(
        new MockQueryDatabase(all_actors = actors_data, all_similarities = similarities_data)
    )
        
    val database = dataset.build()
    emptynessCondition(dataset, dataset.actors)
    
    dataset.actors.keys should be === (Set(1,2,3))
    
    def similarityMatch(id_actor: Int, id_friend: Int, trust: Double) = {
      val actor = dataset.actors(id_actor)
      val friend = dataset.actors(id_friend)
      actor.similarity(friend) should be (Some(trust))
    }
    similarityMatch(1, 2, 1)
    similarityMatch(2, 1, 1)
    similarityMatch(1, 3, 1)
    similarityMatch(3, 1, 1)
    similarityMatch(2, 3, 1)
    similarityMatch(3, 2, 1)
	
  }

}
