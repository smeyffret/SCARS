package fr.cnrs.liris.scars.evaluation.campaign

import fr.cnrs.liris.scars.api._
import fr.cnrs.liris.scars.scorer.ReviewFactory
import scala.actors.Actor._

class ReviewCampaign(scorerBuilder: ReviewFactory, database: Database, val writer: actors.Actor) extends Campaign {

  type Tuple = Review
  
  val score_eval = database.evalReviews

  def compute_score(tuple: Tuple) = {
    def scorer = scorerBuilder()
    val review = tuple
    scorer.scores(review, Set.empty)
  }
  
}
