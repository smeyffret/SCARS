package fr.cnrs.liris.scars.evaluation.dataset

import fr.cnrs.liris.scars.file.epinions.FileDataset
import java.io.File


object FlixsterEvaluation extends FileEvaluation {
  val Values: Seq[Double] = (0.5 to 5.0 by 0.5)
  override def toString = "flixster"
}


object MassaEvaluation extends FileEvaluation {
  val Values: Seq[Double] = (1.0 to 5.0 by 1.0)
  override def toString = "massa"
}


abstract class FileEvaluation {

  val Values: Seq[Double]

  def apply(basepath: String) = {

    val trust_path = file(basepath, "trust_data.txt", 2)

    val (ratings_train_path, ratings_eval_path) = {
      val train_path = basepath + "ratings_data.txt.training"
      val eval_path = basepath + "ratings_data.txt.evaluation"
      val ratings_path = basepath + "ratings_data.txt"
      val train_file = new File(train_path)
      if (train_file.exists)
        (train_path, eval_path)
      else
        (ratings_path, ratings_path)
    }

    val similarity_path = file(basepath, "pearson_data.txt", 2)
    val similarity_item_path = file(basepath, "pearson_items.txt", 2)

    FileDataset.fromFiles(
      ratings_path = ratings_train_path,
      eval_ratings_path = Some(ratings_eval_path),
      trust_path = trust_path,
      similarity_path = similarity_path,
      similarity_item_path = similarity_item_path,
      ratingRange = Values)

  }

  def file(basepath: String, name: String, depth: Int): Option[String] = {
    if (new File(basepath + name).exists)
      Some(basepath + name)
    else if (depth <= 0)
      None
    else
      file(basepath + "/../", name, depth - 1)
  }

}
