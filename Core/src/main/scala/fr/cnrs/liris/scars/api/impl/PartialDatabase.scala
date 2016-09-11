package fr.cnrs.liris.scars.api.impl

import fr.cnrs.liris.scars.api.Database
import fr.cnrs.liris.scars.api.DatabaseBuilder
import fr.cnrs.liris.scars.util.RandomSlicer

class PartialDatabaseBuilder(builder: DatabaseBuilder, percent: Double) extends DatabaseBuilder {
  def build() = new PartialDatabase(builder.build(), percent)
  override def toString = builder.toString + "_" + percent + "%"
}

class PartialDatabase(database: Database, percent: Double) extends Database {
  
  val slicer = RandomSlicer(percent)
  
  def ratingRange = database.ratingRange
  def actors = database.actors
  def items = database.items
  def reviews = database.reviews
  def isLeaveOneOut = database.isInstanceOf
  
  // Fixed evalReviews per experimentation
  val evalReviews = slicer.slice(database.evalReviews)
  
}
