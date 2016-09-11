package fr.cnrs.liris.scars.database.epinions.database

abstract class DBDatabase {

  val reviews_query = "SELECT idreview, iduser, idproduct, rating, review_rating FROM Review"
  val eval_reviews_query = "SELECT idreview, iduser, idproduct, rating, review_rating FROM Review"

  /// must returns categories ordered by hierarchy deep ascending (parents first)
  def categories(f: (Int, String, Int) => Any): Unit

  def actors(f: (Int, Int) => Any): Unit

  def experts(f: (Int, Int, String) => Any): Unit

  def items(f: (Int, String, Int) => Any): Unit

  def trusts(f: (Int, Int, Double) => Any): Unit

  def similarities(f: (Int, Int, Double) => Any): Unit

  def reviews(f: (Int, Int, Int, Double, String) => Any): Unit
  
  def eval_reviews(f: (Int, Int, Int, Double, String) => Any): Unit
  
  def close: Unit

}
