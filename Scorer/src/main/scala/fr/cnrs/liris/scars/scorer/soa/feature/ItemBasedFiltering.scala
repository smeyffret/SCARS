package fr.cnrs.liris.scars.scorer.soa.feature

import fr.cnrs.liris.scars.api._
import fr.cnrs.liris.scars.math._

trait ItemBasedFiltering {
  
  private val util = new Math()
  
  type Similarities = Map[Item, Double]
  private val simBuilder = new SimilarityItemActorMean(positive = true)
  
  def pick_item(actor: Actor, item: Item, without: Set[Review], similarItems: Similarities) = {
    if (similarItems.isEmpty) {
      None
    } else {
      val totalWeight = similarItems.values.sum
      val view = similarItems.view.map{case (item, weight) => weight -> item}
      Probability.selectOneW(view, totalWeight)
    }
  }
  
  def compute_similarity(item: Item, without: Set[Review]) = {
    for {
      (item, other, corr) <- simBuilder.computeSimilarity(item, without)
      sim = similarity(item, other, without, corr)
    } yield (other -> sim)
  }.toMap
  
  def actor_similar_items(actor: Actor, without: Set[Review], similarItems: Similarities) = {
    val actorItems = (actor.reviews -- without).map(_.item)
    similarItems.filterKeys(actorItems.contains)
  }
  
  def compute_similarity_of_actor(actor: Actor, item: Item, without: Set[Review]) = {
    val others = (actor.reviews -- without).map(_.item) - item
    for {
      (item, other, corr) <- simBuilder.computeSimilarityToOthers(item, others, without)
      sim = similarity(item, other, without, corr)
    } yield (other -> sim)
  }.toMap
  
  /**
   * module la similarité en fonction du nombre de notes en commun entre les 2 items
   * eq. 7
   */
  def similarity(item: Item, other: Item, without: Set[Review], corr: Double) = {
    val itemActors = (item.reviews &~ without).map(_.actor)
    val otherActors = (other.reviews &~ without).map(_.actor)
    val commonsRaters = (itemActors & otherActors).size
    val coef = util.sigmoid(commonsRaters / 2.)
    coef * corr
  }
      
}
