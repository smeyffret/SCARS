package fr.cnrs.liris.scars.evaluation.dataset

import fr.cnrs.liris.scars.file.epinions.FileDataset

object SynpinionsEvaluation {

  val Values: Seq[Double] = (1.0 to 5.0 by 1.0)

  def apply(basepath: String) = {

    val trust_path = basepath + "trust_data.txt"
    val ratings_train_path = basepath + "ratings_data.txt.training"
    val ratings_eval_path = basepath + "ratings_data.txt.evaluation"
    val categories_path = basepath + "categories.txt"
    val members_path = basepath + "members.txt"
    val products_path = basepath + "products.txt"

    FileDataset.fromFiles(
      ratings_path = ratings_train_path,
      eval_ratings_path = Some(ratings_eval_path),
      trust_path = Some(trust_path),
//      similarity_path = Some(pearson_path),
      categories_path = Some(categories_path),
      actors_path = Some(members_path),
      items_path = Some(products_path),
      ratingRange = Values)

  }

  override def toString = "synpinions"
}
