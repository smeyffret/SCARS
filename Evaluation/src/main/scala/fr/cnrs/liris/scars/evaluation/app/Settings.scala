package fr.cnrs.liris.scars.evaluation.app

import fr.cnrs.liris.scars.evaluation._
import fr.cnrs.liris.scars.api._
import fr.cnrs.liris.scars.stats._
import fr.cnrs.liris.scars.scorer.Factory
import fr.cnrs.liris.scars.evaluation.scorer._
import fr.cnrs.liris.scars.evaluation.view._
import fr.cnrs.liris.scars.scorer.util._

import EvaluationApp.{time,log}
import scala.collection.SeqView

class Settings(database: Database, decoration: Option[String], withView: Boolean = false, statsMetrics: List[Stats.Value] = List.empty) {

  Stats.defaultMetrics ++= statsMetrics

  def view = {
    if (withView)
      time("views %d", 2){ View(database) }
    else
      View.empty
  }
  
  def buildScorers = Scorers.build(database, this)
  def reloadScorers(path: String) = Scorers.reload(database, this, path)
  def loadScorers(path: String) = Scorers.load(path)
  
  def evaluation(scorers: Iterable[Factory]) = {
    log("\t\t\t\t" + Stats.titles(" "))
    new Evaluation(database, scorers, view)
  }
  
  def compose(factory: Factory) = {
    if (decoration.isDefined)
      factory.compose(decorate)
    else
      factory
  }

  def decorate(scorer: Scorer) = decoration match {
    case Some("segmented") => new SegmentedScorer(scorer, database.ratingRange)
    case Some("leveled") => new LeveledScorer(scorer, database.ratingRange)
    case _ => scorer
  }

  def deltaSettings(ratingRange: Seq[Double]) = {
    val ratings = ratingRange.sorted
    val min = ratings.head
    val max = ratings.last
    val mean = (max + min) / 2
    DeltaSettings(min, max, Some(mean))
  }
  
  def delta = deltaSettings(database.ratingRange)

}
