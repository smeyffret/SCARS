package fr.cnrs.liris.scars.scorer.global

import fr.cnrs.liris.scars.api._
import fr.cnrs.liris.scars.math._
import fr.cnrs.liris.scars.scorer.algo.Correlation

class GlobalCF(correlation: Correlation[Actor]) extends CF {
  
  def this(useCache: Boolean = true) = {
    this(Correlation.actors(useCache, positive = true))
  }

  def scoreSimilars(actor: Actor, item: Item, without: Set[Review]) = {
    correlation.similars(actor, without).toStream.map{ other =>
      for {
        score <- rating(other, item, without)
        sim <- correlation.similarity(actor, other, without)
      } yield (score, sim)
    }
  }
  
  override def toString = "UserBasedCF"

}
