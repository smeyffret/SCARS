package fr.cnrs.liris.scars.scorer.soa.feature

import fr.cnrs.liris.scars.api._
import fr.cnrs.liris.scars.math.SimilarityItem

trait CachedItemBasedFiltering extends ItemBasedFiltering {

  private val simBuilder = new SimilarityItem(positive = true)

  override def compute_similarity(item: Item, without: Set[Review]) = {
    for {
      other <- item.similars
      sim <- similarity(item, other, without)
    } yield other -> sim
  }.toMap
  
  override def compute_similarity_of_actor(actor: Actor, item: Item, without: Set[Review]) = {
    val others = (actor.reviews -- without).map(_.item) - item
    for {
      other <- others
      sim <- similarity(item, other, without)
    } yield other -> sim
  }.toMap
  
  def similarity(item: Item, other: Item, without: Set[Review]) = {
    val common = without & item.reviews & other.reviews
    if (common.isEmpty)
      item.similarity(other)
    else
      simBuilder.similarity(item, other, without)
  }

}
