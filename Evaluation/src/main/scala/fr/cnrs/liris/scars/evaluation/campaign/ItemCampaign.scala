package fr.cnrs.liris.scars.evaluation.campaign

import fr.cnrs.liris.scars.api._
import fr.cnrs.liris.scars.scorer.ItemFactory
import scala.actors.Actor._

class ItemCampaign(scorerBuilder: ItemFactory, database: Database, val writer: actors.Actor) extends Campaign {

  type Tuple = (Item, Iterable[Review])
  
  val score_eval = database.evalReviews.groupBy(_.item)
  
  def compute_score(tuple: Tuple) = {
    def scorer = scorerBuilder()
    val (item, reviews) = tuple
    scorer.scores(item, reviews.toSet, Set.empty)
  }

}
