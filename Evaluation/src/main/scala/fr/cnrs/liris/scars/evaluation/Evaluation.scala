package fr.cnrs.liris.scars.evaluation

import fr.cnrs.liris.scars.api._
import fr.cnrs.liris.scars.evaluation.view._
import fr.cnrs.liris.scars.evaluation.campaign.Campaign
import fr.cnrs.liris.scars.scorer.Factory

class Evaluation(database: Database,
                buildScorer: Iterable[Factory],
                views: View) {

  val eval = new ScorerEvaluation()

  def runScorer() = {
    buildScorer.map{ scorer =>
      evalScore(scorer)
    }
  }

  def savedRunScorer(path: String) = {
    buildScorer.map{ scorer =>
      savedEvalScore(scorer, path)
    }
  }

  def savedEvalScore(scorer: Factory, path: String) = {
    val writer = new FileDataSaver(path + scorer.toString)
    evalScore(scorer, writer)
  }

  def evalScore(scorer: Factory, writer: DataSaver = NoneSaver) = try {
    writer.start()
    val campaign = Campaign(scorer, database, writer)
    lazy val result = campaign.predict_scores()
    val globalStats = eval.stats(scorer.toString, reviews)(result)
    val viewsStats = evalViews(result)
    globalStats :: viewsStats
  } finally {
    writer ! None
  }

  def evalViews(scores: Iterable[(Actor, Item, Double, Option[Score])]) = {
    val partitions = views.partition(scores)
    partitions map { case (kind, scoresView) =>
      eval.stats("_" + kind, reviews)(scoresView)
    }
  }
  
  def reviews = {
    if (database.isLeaveOneOut)
      Nil
    else
      database.reviews
  }

}
