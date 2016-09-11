package fr.cnrs.liris.scars.scorer.global

import fr.cnrs.liris.scars.api._
import fr.cnrs.liris.scars.math._
import fr.cnrs.liris.scars.scorer.algo.Correlation

class ItemBased(correlation: Correlation[Item]) extends CF {
  
  def this(useCache: Boolean = true) = {
    this(Correlation.items(useCache, positive = true))
  }

  def scoreSimilars(actor: Actor, item: Item, without: Set[Review]) = {
    correlation.similars(item, without).toStream.map{ other =>
      for {
        score <- rating(actor, other, without)
        sim <- correlation.similarity(item, other, without)
      } yield (score, sim)
    }
  }
  
  override def toString = "ItemBasedCF"

}
