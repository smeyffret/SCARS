package fr.cnrs.liris.scars.evaluation.campaign

import fr.cnrs.liris.scars.api._
import fr.cnrs.liris.scars.scorer.ActorFactory
import scala.actors.Actor._

class ActorCampaign(scorerBuilder: ActorFactory, database: Database, val writer: actors.Actor) extends Campaign {

  type Tuple = (Actor, Iterable[Review])
  
  val score_eval = database.evalReviews.groupBy(_.actor)
  
  def compute_score(tuple: Tuple) = {
    def scorer = scorerBuilder()
    val (actor, reviews) = tuple
    scorer.scores(actor, reviews.toSet, Set.empty)
  }

}
