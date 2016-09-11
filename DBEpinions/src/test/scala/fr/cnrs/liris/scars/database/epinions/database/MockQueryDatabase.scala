package fr.cnrs.liris.scars.database.epinions.database

class MockQueryDatabase(
  all_categories: Seq[(Int, String, Int)] = Nil,	// id, name, parent (0 as null)
  all_actors: Seq[(Int, Int)] = Nil,				// id, rank (0 as null)
  all_experts: Seq[(Int, Int, String)] = Nil,			// idactor, idcategory, level
  all_items: Seq[(Int, String, Int)] = Nil,			// id, name, idcategory
  all_trusts: Seq[(Int, Int, Double)] = Nil,		// idactor, idfriend, trust
  all_similarities: Seq[(Int, Int, Double)] = Nil,	// idactor, idsimilar, similarity
  //    all_eval_reviews: Seq[(Int, Int, Int, Double, String)] = Nil,
  all_reviews: Seq[(Int, Int, Int, Double, String)] = Nil) extends DBDatabase {

  /// Input categories must be order by hierarchy deep ascending (parents firt)
  def categories(f: (Int, String, Int) => Any) {
    all_categories foreach Function.tupled(f)
  }

  def parentCategories(f: (Int, String) => Any) {
    all_categories.foreach {
      case (id, name, 0) => f(id, name)
    }
  }

  def categoriesWithParent(idparent: Int)(f: (Int, String) => Any) {
    all_categories.foreach {
      case (id, name, `idparent`) => f(id, name)
    }
  }

  def actors(f: (Int, Int) => Any) {
    all_actors.foreach(Function.tupled(f))
  }

  def experts(f: (Int, Int, String) => Any) {
    all_experts.foreach(Function.tupled(f))
  }

  def items(f: (Int, String, Int) => Any) {
    all_items.foreach(Function.tupled(f))
  }

  def trusts(f: (Int, Int, Double) => Any) {
    all_trusts.foreach(Function.tupled(f))
  }

  def similarities(f: (Int, Int, Double) => Any) {
    all_similarities.foreach(Function.tupled(f))
  }

  def reviews(f: (Int, Int, Int, Double, String) => Any) {
    all_reviews.foreach(Function.tupled(f))
  }
  
  def eval_reviews(f: (Int, Int, Int, Double, String) => Any) {
    all_reviews.foreach(Function.tupled(f))
  }
  
  def close {}

}
