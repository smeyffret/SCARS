package fr.cnrs.liris.scars.evaluation.analyse

import fr.cnrs.liris.scars.api.Database
import fr.cnrs.liris.scars.scorer.social.impl.RecSocialScorer
import fr.cnrs.liris.scars.scorer.social.feature.Trust
import fr.cnrs.liris.scars.scorer.social.parent.SingleParent

object Process extends Enumeration("stats", "actors", "friends", "extended", "items") {
  
  val CharacteristicsProcess, ActorsPearson, FriendsPearson, ExtendedPearson, ItemsPearson, Nothing = Value
  type Process = Value
  
  def analyse(database: Database, process: Process) = process match {
    case CharacteristicsProcess =>
      new Characteristics(database).show(2)
      sys.exit()
    case ActorsPearson =>
      SimilarityBuilder.buildActors(database, "pearson_values.txt", unique = true)
      sys.exit()
    case FriendsPearson =>
      SimilarityBuilder.buildActors(database, "pearson_values.txt", unique = true, onlyFriends = true)
      sys.exit()
    case ExtendedPearson =>
      val n = 1
      val scorer = new RecSocialScorer(1, n) with SingleParent with Trust
      SimilarityBuilder.buildExtended(database, "extended%d_pearson_data.txt".format(n), scorer, unique = true, onlyFriends = true)
      sys.exit()
    case ItemsPearson =>
      SimilarityBuilder.buildItems(database, "pearson_items.txt")
      sys.exit()
    case Nothing =>
  }
  
}
