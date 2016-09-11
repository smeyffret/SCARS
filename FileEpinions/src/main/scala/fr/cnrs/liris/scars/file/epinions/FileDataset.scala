package fr.cnrs.liris.scars.file.epinions

import fr.cnrs.liris.scars.api._
import fr.cnrs.liris.scars.api.impl._
import io.Source
import collection.mutable.ListBuffer

object FileDataset {

  def apply(ratings_source: Source,
    eval_ratings_source: Option[Source] = None,
    trust_source: Option[Source] = None,
    similarity_source: Option[Source] = None,
    similarity_item_source: Option[Source] = None,
    categories_source: Option[Source] = None,
    actors_source: Option[Source] = None,
    items_source: Option[Source] = None,
    ratingRange: Seq[Double] = Nil) = {
    new FileDataset(ratings_source, 
      eval_ratings_source.getOrElse(ratings_source), 
      trust_source, similarity_source, similarity_item_source,
      categories_source, 
      actors_source, items_source)(
      ratingRange)
  }

  def fromFiles(ratings_path: String,
    eval_ratings_path: Option[String] = None,
    trust_path: Option[String] = None,
    similarity_path: Option[String] = None,
    similarity_item_path: Option[String] = None,
    categories_path: Option[String] = None,
    actors_path: Option[String] = None,
    items_path: Option[String] = None,
    ratingRange: Seq[Double] = Nil) = {
    val ratings_source = Source.fromFile(ratings_path)
    val eval_ratings_source =
      if (eval_ratings_path.getOrElse(ratings_path) == ratings_path)
        ratings_source
      else
        Source.fromFile(eval_ratings_path.get)
    new FileDataset(ratings_source,
      eval_ratings_source,
      trust_path.map(Source.fromFile),
      similarity_path.map(Source.fromFile),
      similarity_item_path.map(Source.fromFile),
      categories_path.map(Source.fromFile),
      actors_path.map(Source.fromFile),
      items_path.map(Source.fromFile))(
      ratingRange)
  }

}

class FileDataset(
  ratings_source: Source,
  eval_ratings_source: Source,
  trust_source: Option[Source] = None,
  similarity_source: Option[Source] = None,
  similarity_item_source: Option[Source] = None,
  categories_source: Option[Source] = None,
  actors_source: Option[Source] = None,
  items_source: Option[Source] = None)
  (ratingRange: Seq[Double]) extends DatabaseBuilder {

  var failures = collection.mutable.Map[Source, (ListBuffer[String], String)]()

  def addFailure(source: Source, line: String, e: Throwable) {
    failures.getOrElseUpdate(source, (ListBuffer[String](), e.getMessage))._1 += line
  }

  var categories = collection.mutable.Map[Int, DefaultCategory]()
  var actors = collection.mutable.Map[Int, DefaultActor]()
  var items = collection.mutable.Map[Int, DefaultItem]()

  def getCategory(id: Int) = {
    categories.getOrElseUpdate(id, new DefaultCategory(id, "Category" + id))
  }

  def getActor(id: Int) = {
    actors.getOrElseUpdate(id, new DefaultActor(id))
  }

  def getItem(id: Int) = {
    items.getOrElseUpdate(id, new DefaultItem(id, "Item" + id))
  }

  private def parseOptionnalSource(source: Option[Source])(f: PartialFunction[List[String], Unit]) = {
    source.foreach { s => parseSource(s)(f) }
  }

  private def parseSource(source: Source)(f: PartialFunction[List[String], Unit]) = {
    source.getLines.filterNot(_.startsWith("#")).foreach { line =>
      val args = line.trim.split("""\s+""").toList
      try {
        f(args)
      } catch {
        case e => addFailure(source, line, e)
      }
    }
  }

  def build() = {

    parseOptionnalSource(categories_source) {
      case id :: Nil => getCategory(id.toInt)
    }

    parseOptionnalSource(actors_source) {
      case id_actor :: id_categories =>
        val actor = getActor(id_actor.toInt)
        id_categories.foreach { id =>
          actor addExpertise (getCategory(id.toInt), Expertise.Leader)
        }
    }

    parseOptionnalSource(items_source) {
      case id_product :: id_category :: Nil =>
        val category = getCategory(id_category.toInt)
        val item = getItem(id_product.toInt)
        category addItem item
    }

    parseOptionnalSource(trust_source) {
      case id_actor :: id_friend :: trust :: Nil =>  // unidirectionnal relation
        val actor = getActor(id_actor.toInt)
        val friend = getActor(id_friend.toInt)
        actor addTrust (friend, trust.toDouble)
        friend addFollower actor
      case id_actor :: id_friend :: Nil =>  // bidirectionnal relation
        val actor = getActor(id_actor.toInt)
        val friend = getActor(id_friend.toInt)
        actor addTrust (friend, 1.0)
        friend addTrust (actor, 1.0)
        actor addFollower friend
        friend addFollower actor
    }

    parseOptionnalSource(similarity_source) {
      case id_actor :: id_similar :: similarity :: Nil =>
        val actor = getActor(id_actor.toInt)
        val similar = getActor(id_similar.toInt)
        actor addSimilarity (similar, similarity.toDouble)
        similar addSimilarity (actor, similarity.toDouble)
    }

    parseOptionnalSource(similarity_item_source) {
      case id_item :: id_similar :: similarity :: Nil =>
        val item = getItem(id_item.toInt)
        val similar = getItem(id_similar.toInt)
        item addSimilarity (similar, similarity.toDouble)
        similar addSimilarity (item, similarity.toDouble)
    }

    val leave_one_out = (eval_ratings_source == ratings_source)
    val reviews = readReviews(ratings_source, true)
    val evalReviews =
      if (leave_one_out)
        reviews
      else
        readReviews(eval_ratings_source, false)

    //    reviews.foreach { review =>
    //      review.actor.asInstanceOf[DefaultActor].reviews += review
    //      review.item.asInstanceOf[DefaultItem].reviews += review
    //    }

    new DefaultDatabase(
      ratingRange,
      actors.values.toList,
      items.values.toList,
      reviews.toList,
      evalReviews.toList,
      leave_one_out)

  }

  def readReviews(ratings_source: Source, addReview: Boolean) = {
    val reviews = ListBuffer[Review]()
    parseSource(ratings_source) {
      case id_actor :: id_item :: Nil =>
        // nothing
      case id_actor :: id_item :: rating :: remainder =>
        val actor = getActor(id_actor.toInt)
        val item = getItem(id_item.toInt)
        val (confidence, date) = confidenceDate(remainder)
        val review = new DefaultReview(actor, item, rating.toDouble, confidence, date)
        reviews += review
        if (addReview) {
          actor add review
          item add review
        }
    }
    reviews.toList
  }

  override def toString = "FileDataset"
  
  private val format = new java.text.SimpleDateFormat("yyyy-MM-dd")
  private def confidenceDate(remainder: List[String]) = remainder match {
    case Nil => 
      1. -> None
    case x :: Nil => try {
        x.toDouble -> None
      } catch {
        case _: NumberFormatException =>
          1. -> Some(format.parse(x))
      }
    case x :: y :: Nil =>
      x.toDouble -> Some(format.parse(y))
  }

  //  private def parseFile(path: String)(f: (Int, Int, Double) => Unit) {
  //    val scanner = new Scanner(new java.io.BufferedReader(new java.io.FileReader(path)))
  //    scanner.useLocale(java.util.Locale.ENGLISH)
  //    while (scanner.hasNext) {
  //      try{
  //        // val (actor, item, rating) = (scanner.nextInt, scanner.nextInt, scanner.nextDouble)
  //        f(scanner.nextInt, scanner.nextInt, scanner.nextDouble)
  //      } catch {
  //        case _ =>
  //      }
  //    }
  //    scanner.close
  //  }

}
