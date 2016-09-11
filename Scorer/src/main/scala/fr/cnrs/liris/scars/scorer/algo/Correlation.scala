package fr.cnrs.liris.scars.scorer.algo

import fr.cnrs.liris.scars.api._
import fr.cnrs.liris.scars.math._

object Correlation {
  
  type Correlable[T] = Identifiable with Reviews with Similarities[T]
  
  def actors(useCache: Boolean = true, positive: Boolean = false) = {
    new Correlation[Actor](useCache, new SimilarityActor(positive))
  }
  
  def items(useCache: Boolean = true, positive: Boolean = false) = {
    new Correlation[Item](useCache, new SimilarityItem(positive))
  }
  
}

class Correlation[Subject <: Correlation.Correlable[Subject]]
          (useCache: Boolean, simBuilder: Similarity[Subject]) {
  
  def similars(subject: Subject, without: Set[Review]) = {
    if (useCache)
      subject.similars
    else
      simBuilder.commonRaters(subject, without)
  }

  def similarity(subject: Subject, other: Subject, without: Set[Review]) = {
    def common = without & subject.reviews & other.reviews
    if (useCache && common.isEmpty)
      subject.similarity(other)
    else
      simBuilder.similarity(subject, other, without)
  }
    
}
