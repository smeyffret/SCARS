package fr.cnrs.liris.scars.evaluation.scorer

import fr.cnrs.liris.scars.api._
import fr.cnrs.liris.scars.evaluation.app._
import fr.cnrs.liris.scars.scorer.Factory

object Scorers {
  
  def load(path: String): Iterable[Factory] = {
    Loader.loadDir(path).map(Factory.singleton(_))
  }
  
  def reload(database: Database, settings: Settings, path: String): Iterable[Factory] = {
    for {
      builder <- build(database, settings)
    } yield Loader.compose(path, builder.instance) match {
      case Some(scorer) => Factory.singleton(scorer)
      case None => builder
    }
  }
  
  def build(database: Database, settings: Settings): Iterable[Factory] = {
    Instances(database, settings).map(Factory(_))
  }

}
