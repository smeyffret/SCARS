package fr.cnrs.liris.scars.evaluation.app

import fr.cnrs.liris.scars.evaluation._
import fr.cnrs.liris.scars.evaluation.scorer._
import fr.cnrs.liris.scars.scorer._
import fr.cnrs.liris.scars.api._
import fr.cnrs.liris.scars.stats._

class EvaluationRun(settings: Settings, load: Option[String], save: Option[String]) {

  def run() {
    val e = settings.evaluation(getScorers())
    val stats = runEvaluation(e)
//    stats.force // do all the work
  }
  
  
  def getScorers(): Iterable[Factory] = {
    (load, save) match {
      case (Some(path), Some(_)) => settings.reloadScorers(path)
      case (Some(path), None) => Loader.loadDir(path).map(new SingletonFactory(_))
      case (None, _) => settings.buildScorers
    }
  }.map{settings.compose}
  
  def runEvaluation(e: Evaluation) = {
    if (save.isDefined) {
      e.savedRunScorer(save.get)
    } else {
      e.runScorer()
    }
  }

}
