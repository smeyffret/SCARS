package fr.cnrs.liris.scars.evaluation.scorer

import fr.cnrs.liris.scars.api._
import fr.cnrs.liris.scars.scorer.util._
import java.io.File

object Loader {

  /**
   * Compose the scorer with the file if the file exists
   * The path can be a dir if a file with the name 'dir/scorer.toString' exists
   * If the file doesn't exist, return the scorer without composing it
   */
  def compose(path: String, scorer: Scorer) = {
    val file = scorerFile(path, scorer)
    if (file.exists) {
      Some(new CompositeScorer(load(file), scorer) {
        override def toString = "_" + scorer.toString
      })
    } else {
      None
    }
  }
  
  private def scorerFile(path: String, scorer: Scorer) = {
    val file = new File(path)
    if (file.isDirectory) {
      new File(path + "/" + scorer.toString)
    } else if (file.exists) {
      file
    } else {
      new File(path + scorer.toString)
    }
  }

  /**
   * Load all scorer files identified by the paths
   * If a path denotes a directory, all files in that directory are loaded
   * return a list of scorers
   */
  def loadDir(paths: String*) = {
    val files = for {
      path <- paths
      file <- pathToFiles(path)
    } yield (file)
    files map load
  }
  
  /**
   * load only one single file (must not be a directory)
   * return a scorer
   */
  def loadFile(path: String) = {
    load(new File(path))
  }
  
  def load(file: File) = {
    RatingsScorer(file)
  }
  
  private def pathToFiles(path: String) = {
    val f = new File(path)
    if (f.isFile) {
      List(f)
    } else {
      f.listFiles.toList.filterNot(_.isDirectory)
    }
  }
  
}
