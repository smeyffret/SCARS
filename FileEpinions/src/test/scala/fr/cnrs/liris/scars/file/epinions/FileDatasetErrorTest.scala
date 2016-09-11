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
class FileDatasetErrorTest extends FunSuite with ShouldMatchers with FileDatasetCondition {
  
  test("Parsing error categories should return no categories") {
    val categories_list = List("Category1", "2 3", "")
    val categories_source = Some(fromString(categories_list.mkString("\n")))
    val dataset = FileDataset(
        empty_ratings_source,
        categories_source = categories_source
    )
        
    val database = dataset.build()
    emptynessCondition(dataset, dataset.failures)
    
    dataset.failures.keys should be === (Set(categories_source.get))
    
    val failures = dataset.failures(categories_source.get)._1
    failures should be === (List("Category1", "2 3")) // last element trimmed
  }

  test("Parsing commented categories should return no categories") {
    val categories_list = List("#1", "#2", "#3")
    val categories_source = Some(fromString(categories_list.mkString("\n")))
    val dataset = FileDataset(
        empty_ratings_source,
        categories_source = categories_source
    )
        
    val database = dataset.build()
    emptynessCondition(dataset)
  }

  test("Parsing commented categories in error should return no categories and no failure") {
    val categories_list = List("# Category1", "#2 3", "#")
    val categories_source = Some(fromString(categories_list.mkString("\n")))
    val dataset = FileDataset(
        empty_ratings_source,
        categories_source = categories_source
    )
        
    val database = dataset.build()
    emptynessCondition(dataset)
  }
  
}