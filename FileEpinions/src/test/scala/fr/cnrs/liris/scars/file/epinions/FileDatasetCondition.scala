package fr.cnrs.liris.scars.file.epinions

import org.scalatest.matchers.ShouldMatchers
import io.Source.fromString
import fr.cnrs.liris.scars.api.Database

trait FileDatasetCondition extends ShouldMatchers {

  var empty_ratings_source = fromString("")
  
  def emptynessCondition(dataset: FileDataset, excepts: Iterable[Any]*) {
    val empty = List(dataset.categories, dataset.actors, dataset.items, dataset.failures)
    emptynessCondition(empty, excepts: _*)
  }

  def emptynessCondition(database: Database, excepts: Iterable[Any]*) {
    val empty = List(database.reviews, database.actors, database.items, database.evalReviews)
    emptynessCondition(empty, excepts: _*)
  }
  
  def emptynessCondition(empty: List[Iterable[Any]], excepts: Iterable[Any]*) {
    excepts.foreach{ col =>
      col should not be ('empty)
    }
    empty.filterNot(excepts.contains).foreach{ col =>
      col should be ('empty)
    }
  }

}