package fr.cnrs.liris.scars.evaluation.campaign

import fr.cnrs.liris.scars.api._
import scala.actors.Actor._
import fr.cnrs.liris.scars.scorer._

object Campaign {
  def apply(factory: Factory, database: Database, writer: actors.Actor) = factory match {
    case f: ActorFactory =>
      new ActorCampaign(f, database, writer)
    case f: ItemFactory =>
      new ItemCampaign(f, database, writer)
    case f: ReviewFactory => // no specialization
      new ReviewCampaign(f, database, writer)
  }
}

trait Campaign {
  
  val writer: actors.Actor

  type Tuple
  
  val score_eval: Iterable[Tuple]
  
  /**
   * @return the stats of the scorer.
   */
  def predict_scores(): Iterable[(Actor, Item, Double, Option[Score])] = {
    score_eval.par.flatMap{ tuple =>
      val begin = System.currentTimeMillis
      val scores = compute_score(tuple)
      val time = (System.currentTimeMillis - begin).toDouble / scores.size
      for {
        (review, rawScore) <- scores
        score = rawScore.map(_.copy(time = time))
      } yield write(review, score)
    }.seq
  }
  
  def compute_score(tuple: Tuple): Map[Review, Option[Score]]

  def write(review: Review, score: Option[Score]) = {
    import review._
    writer ! (actor, item, score) 
    (actor, item, rating, score)
  }
}
