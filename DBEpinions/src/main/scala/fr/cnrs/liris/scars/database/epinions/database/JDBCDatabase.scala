package fr.cnrs.liris.scars.database.epinions.database

import java.sql._

object JDBCDatabase {

  def apply(host: String, database: String, username: String, password: String, port: Int = 3306, queries: Query = EpinionsQuery) = {
    val url = "jdbc:mysql://%s:%d/%s".format(host, port, database)
    Class.forName("com.mysql.jdbc.Driver")
    new JDBCDatabase ( DriverManager.getConnection(url, username, password), queries )
  }

}

class JDBCDatabase(db: Connection, queries: Query) extends DBDatabase {

  val stm = db.createStatement
  
  val categoriesStm = db.prepareStatement(queries.categoriesQuery)
  val parentCategoriesStm = db.prepareStatement(queries.parentCategoriesQuery)
  val categoriesWithParentStm = db.prepareStatement(queries.categoriesWithParentQuery)
  val actorsStm = db.prepareStatement(queries.actorsQuery)
  val expertsStm = db.prepareStatement(queries.expertsQuery)
  val itemsStm = db.prepareStatement(queries.itemsQuery)
  val trustsStm = db.prepareStatement(queries.trustsQuery)
  val similaritiesStm = db.prepareStatement(queries.similaritiesQuery)
  val reviewsStm = db.prepareStatement(queries.reviewsQuery)
  val evalReviewsStm = db.prepareStatement(queries.evalReviewsQuery)

  def categories(f: (Int, String, Int) => Any) {
    val row = categoriesStm.executeQuery()
    while (row.next){
      f(row.getInt(1), row.getString(2), row.getInt(3))
    }
  }

  def parentCategories(f: (Int, String) => Any) {
    val row = parentCategoriesStm.executeQuery()
    while (row.next){
      f(row.getInt(1), row.getString(2))
    }
  }

  def categoriesWithParent(id: Int)(f: (Int, String) => Any) {
    categoriesWithParentStm.setInt(id, 0)
    val row = categoriesWithParentStm.executeQuery()
    while (row.next){
      f(row.getInt(1), row.getString(2))
    }
  }

  def actors(f: (Int, Int) => Any) {
    val row = actorsStm.executeQuery()
    while (row.next){
      f(row.getInt(1), row.getInt(2))
    }
  }

  def experts(f: (Int, Int, String) => Any) {
    val row = expertsStm.executeQuery()
    while (row.next){
      f(row.getInt(1), row.getInt(2), row.getString(3))
    }
  }

  def items(f: (Int, String, Int) => Any) {
    val row = itemsStm.executeQuery()
    while (row.next){
      f(row.getInt(1), row.getString(2), row.getInt(3))
    }
  }

  def trusts(f: (Int, Int, Double) => Any) {
    val row = trustsStm.executeQuery()
    while (row.next){
      f(row.getInt(1), row.getInt(2), row.getDouble(3))
    }
  }

  def similarities(f: (Int, Int, Double) => Any) {
    val row = similaritiesStm.executeQuery()
    while (row.next){
      f(row.getInt(1), row.getInt(2), row.getDouble(3))
    }
  }

  def reviews(f: (Int, Int, Int, Double, String) => Any) {
    reviews_query(reviewsStm)(f)
  }
  def eval_reviews(f: (Int, Int, Int, Double, String) => Any) {
    reviews_query(evalReviewsStm)(f)
  }

  private def reviews_query(query: PreparedStatement)(f: (Int, Int, Int, Double, String) => Any) {
    val row = query.executeQuery()
    while (row.next){
      f(row.getInt(1), row.getInt(2), row.getInt(3), row.getDouble(4), row.getString(5))
    }
  }
  
  def close {
    db.close
  }

}
