package fr.cnrs.liris.scars.database.epinions

import org.scalatest.matchers.ShouldMatchers
import fr.cnrs.liris.scars.api.Database

trait DBDatasetCondition extends ShouldMatchers {

  val empty_reviews_data = Nil

  def emptynessCondition(dataset: DBDataset, excepts: Iterable[Any]*) {
    val empty = List(dataset.categories, dataset.actors, dataset.items)
    emptynessCondition(empty, excepts: _*)
  }

  def emptynessCondition(database: Database, excepts: Iterable[Any]*) {
    val empty = List(database.reviews, database.actors, database.items, database.evalReviews)
    emptynessCondition(empty, excepts: _*)
  }

  def emptynessCondition(empty: List[Iterable[Any]], excepts: Iterable[Any]*) {
    excepts.foreach { col =>
      col should not be ('empty)
    }
    empty.filterNot(excepts.contains).foreach { col =>
      col should be('empty)
    }
  }

}