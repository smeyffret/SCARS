package fr.cnrs.liris.scars.evaluation.dataset

import fr.cnrs.liris.scars.database.epinions._

object DBEpinionsEvaluation extends DBEvaluation {
  val Values: Seq[Double] = (1.0 to 5.0 by 1.0)
  override def toString = "massa"
}

abstract class DBEvaluation {

  val Values: Seq[Double]
  
  def fromJDBC(host: String = "127.0.0.1", database: String = "epinions", user: String = "root", pwd: String = "") = {
    DBDataset.fromDatabase(host, database, user, pwd, Values)
  }

}

