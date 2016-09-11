package fr.cnrs.liris.scars.api

abstract class DatabaseBuilder {
  def build(): Database
}

abstract class Database {
  
  /**
   *  Sequence of possible ratings, ordered from the worst rating to the best
   *  For example if a rating is an integer between 1 and 5 inclusive
   *  should return List(1, 2, 3, 4, 5)
   */
  def ratingRange: Seq[Double]

  /**
   * List of all actors in the database
   */
  def actors: Iterable[Actor]
  
  /**
   * List of all items in the database
   */
  def items: Iterable[Item]
  
  /**
   * List of all reviews contained in the training set, ie. known by the system
   * Those reviews are known by actors and items
   */
  def reviews: Iterable[Review]
  
  /**
   * List of all reviews contained in the evaluation set
   * Those reviews are known by nothing
   */
  def evalReviews: Iterable[Review]

  /**
   * True if it is a "leave one out" evaluation campaign, ie. reviews == evalReviews
   * otherwithe, the intersection between reviews and evalReviews should be empty
   */
  def isLeaveOneOut: Boolean

}
