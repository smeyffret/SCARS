package fr.cnrs.liris.scars.evaluation.analyse

import fr.cnrs.liris.scars.evaluation._
import fr.cnrs.liris.scars.api._
import fr.cnrs.liris.scars.math._
import scala.collection.GenIterable

object SimilarityBuilder {
  
  def buildActors(database: Database, path: String, unique: Boolean = true, positive: Boolean = true, onlyFriends: Boolean = false) {
    buildSims(path, database.actors.par)(new SimilarityActor(positive, unique, onlyFriends))
  }

  def buildExtended(database: Database, path: String, scorer: Scorer, unique: Boolean = true, positive: Boolean = true, onlyFriends: Boolean = false) {
    buildSims(path, database.actors.par)(new ExtendedSimilarityActor(scorer, positive, unique, onlyFriends))
  }

  def buildItems(database: Database, path: String, unique: Boolean = true, positive: Boolean = true) {
    buildSims(path, database.items.par)(new SimilarityItem(positive, unique))
  }
  
  private def buildSims[Subject <: Identifiable](path: String, subjects: GenIterable[Subject])(builder: => Similarity[Subject]) {
    save(path) { saver =>
      for {
        subject <- subjects
        sim <- builder.computeSimilarity(subject)
      }{
        saver ! sim
      }
    }
  }
  
  private def save(path: String)(f: FileDataSaver => Unit) {
    val fileSaver = new FileDataSaver(path)
    fileSaver.start()
    f(fileSaver)
    fileSaver ! None
  }

}
