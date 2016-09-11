package fr.cnrs.liris.scars.database.epinions

import fr.cnrs.liris.scars.api._
import fr.cnrs.liris.scars.api.impl._
import collection.mutable.ListBuffer
import database._

object DBDataset {

  def fromDatabase(host: String, database: String, username: String, password: String,
                   ratingRange: Seq[Double] = Nil) = {
    DBDataset(JDBCDatabase(host, database, username, password), ratingRange)
  }

  def apply(db: DBDatabase, ratingRange: Seq[Double] = Nil) = {
    new DBDataset(db)(ratingRange)
  }

}

class DBDataset(db: DBDatabase)(ratingRange: Seq[Double]) extends DatabaseBuilder {

  var categories = collection.mutable.Map[Int, DefaultCategory]()
  var actors = collection.mutable.Map[Int, DefaultActor]()
  var items = collection.mutable.Map[Int, DefaultItem]()

  def getActor(id: Int) = {
    actors.getOrElseUpdate(id, {
      new DefaultActor(id)
    })
  }

  def getItem(id: Int, name: String = "", idcategory: Int = -1) = {
    items.getOrElseUpdate(id, {
      val category = getCategory(idcategory)
      val item = new DefaultItem(id, name)
      category addItem item
      item
    })
  }

  def getCategory(id: Int, name: String = "") = {
    categories.getOrElseUpdate(id, {
      new DefaultCategory(id, name)
    })
  }


  def build() = try {

    db.categories { (id, name, idparent) =>
      val category = getCategory(id, name)
      if (idparent > 0) {
        val parent = getCategory(idparent)
        parent addChild category
      }
    }

    db.actors { (id, rank) =>
      val actor = getActor(id)
      if (rank != 0)
        actor.rank = Some(rank)
    }
    
    db.experts { (iduser, idcategory, expertise) =>
      val actor = getActor(iduser)
      val category = getCategory(idcategory)
      val expertiseValue = Expertise.withName(expertise)
      actor.addExpertise(category, expertiseValue)
    }
    
    db.items { (id, name, idcategory) =>
      getItem(id, name, idcategory)
    }
    
    db.trusts { (iduser, id_trusted, trust) =>
      val actor = getActor(iduser)
      val friend = getActor(id_trusted)
      actor.addTrust(friend, trust)
      friend.addFollower(actor)
    }
    
    db.similarities { (iduser, idsimilar, similarity) =>
      val actor = getActor(iduser)
      val similar = getActor(idsimilar)
      actor.addSimilarity(similar, similarity)
      similar.addSimilarity(actor, similarity)
    }
    

    val database_reviews = db.reviews_query
    val database_eval_reviews = db.eval_reviews_query

    val reviews = readReviews()
    val evalReviews =
      if (database_eval_reviews == database_reviews)
        reviews
      else
        readEvalReviews()

    new DefaultDatabase(
      ratingRange,
      actors.values,
      items.values,
      reviews,
      evalReviews,
      true) // always leave_one_out with database for now

  } finally {
    db.close
  }

  def readReviews() = {
    val result = ListBuffer[DefaultReview]()
    db.reviews { (id, iduser, idproduct, rating, review_rating) =>
      result += parse_review(true)(id, iduser, idproduct, rating, review_rating)
    }
    result.toList
  }

  def readEvalReviews() = {
    val result = ListBuffer[DefaultReview]()
    db.eval_reviews { (id, iduser, idproduct, rating, review_rating) =>
      result += parse_review(false)(id, iduser, idproduct, rating, review_rating)
    }
    result.toList
  }
  
  private def parse_review(add: Boolean)
      (id: Int, iduser: Int, idproduct: Int, rating: Double, review_rating: String) = {
    val actor = getActor(iduser)
    val item = getItem(idproduct)
    val reviewRating = ReviewRating.withName(review_rating).asInstanceOf[ReviewRating.ReviewRatingValue]
    val review = new DefaultReview(actor, item, rating, reviewRating.weight)
    if (add) {
      actor add review
      item add review
    }
    review
  }

  override def toString = "QueryDBDataset"

}
