package fr.cnrs.liris.scars.scorer.soa

import fr.cnrs.liris.scars.api._
import fr.cnrs.liris.scars.util.Threshold
import fr.cnrs.liris.scars.math._
import fr.cnrs.liris.scars.scorer.algo.Aggregation

class TrustWalker(depth: Int) extends Scorer with Aggregation with Threshold with feature.ItemBasedFiltering {

  require(depth >= 0, "depth=%d must be positive".format(depth))

  private val util = new Math()
  val random = new scala.util.Random()
  val proba = Probability
  
  val MaxTry = 10000
  val Epsilon = 0.0001

  def score(actor: Actor, item: Item, without: Set[Review]) = {
    val sims = compute_similarity(item, without)
    multiple_random_walks(random_walk(actor, item, without, 0, sims))
  }
  
  def multiple_random_walks(score: => Option[Score]) = {
    var delta = Double.NaN
    var lastVariance = Double.NaN
    val (walks, nbTries) = iterate(score){ walks =>
      util.variance(walks.map(_.value)).foreach{ variance =>
        delta = math.abs(lastVariance - variance)
        lastVariance = variance
      }
      delta < Epsilon
    }
    if (nbTries == MaxTry) // fail
      None
    else {
      val scores = for {
        score <- walks
      } yield Some(score -> 1.0)
      aggregate(scores)
    }
  }
  
  @scala.annotation.tailrec
  final def random_walk(actor: Actor, item: Item, without: Set[Review], k: Int, similarItems: Similarities): Option[Score] = {
    val score0 = rating(actor, item, without) //.orElse not usable with tailrec optimisation
    if (score0.isDefined) {
      score0.map( _.copy(actors = k))
    } else{
      val actorSimilarItems = actor_similar_items(actor, without, similarItems)
      val stayHere = stay_here(actor, item, without, k, actorSimilarItems)
      if (stayHere) {
        pick_item(actor, item, without, actorSimilarItems).flatMap{ case (corr, similarItem) =>
          rating(actor, similarItem, without).map(_.copy(actors = k, confidence = corr))
        }
      } else {
        val friend = pick_friend(actor)
        if (friend.isEmpty) {
          None
        } else {
          random_walk(friend.get, item, without, k + 1, similarItems)
        }
      }
    }
  }
  
  def stay_here(actor: Actor, item: Item, without: Set[Review], k: Int, sims: Similarities) = {
    k == depth ||
      (!sims.isEmpty && random.nextDouble < proba_picking_similar_item(sims, k))
  }
  
  def proba_picking_similar_item(sims: Similarities, k: Int) = {
    val maxSim = sims.values.max
    val coef = util.sigmoid(k / 2.)
    maxSim * coef
  }
  
  def pick_friend(actor: Actor) = {
    val friends = actor.friends
    proba.selectUniformaly(friends, 1).headOption
  }
  
  override def toString = "TrustWalker" + depth
  
}

class RandomWalk(depth: Int) extends TrustWalker(depth) {
  override def compute_similarity(item: Item, without: Set[Review]) = Map.empty
  override def toString = "RandomWalk" + depth
}
