package fr.cnrs.liris.scars.math

import fr.cnrs.liris.scars.api._
/**
 * @author  Simon Meyffret
 * @version 0.0.1
 * @since   scala 2.8
 * Date: 3 fev. 2011
 * Time: 13:06:02
 */

abstract class SimilarityImpl[Subject <: Identifiable, Other <: Identifiable]
                      (positive: Boolean, uniqueId: Boolean) extends Similarity[Subject] {
  // possibilité de doublons dans le résultat : convertir en Set pour les supprimer

  protected val util = new Math()

  def unique(subject: Subject)(in: Set[Subject]) = in.filter( subject.id < _.id )
  
  def filters(subject: Subject) = {
    if (uniqueId) {
      unique(subject)_
    } else {
      identity[Set[Subject]]_
    }
  }

  def computeSimilarity(subject: Subject, without: Set[Review] = Set.empty) = {
    val others = filters(subject)(commonRaters(subject, without))
    computeSimilarityToOthers(subject, others, without)
  }

  def similarity(subject: Subject, other: Subject,
                  without: Set[Review] = Set.empty,
                  default: Option[Double] = None) = {
    for {
      sim <- raw_similarity(subject, other, without) if checkPositive(sim)
    } yield (borne(sim))
  }.orElse(default)
  
  def raw_similarity(subject: Subject, other: Subject, without: Set[Review] = Set.empty) = {
    val subjectRatings = ratingsMap(subject, without)
    val otherRatings = ratingsMap(other, without)
    val join = mergeRatings(subject, subjectRatings)(other, otherRatings)
    util.pearsonCorrelationCoefficient(join)
  }
  
  def ratingsMap(subject: Subject, without: Set[Review]): Map[Other, Double]
  
  def mergeRatings(subject: Subject, subjectRatings: Map[Other, Double])
                  (other: Subject, otherRatings: Map[Other, Double]) = {
    util.unionMaps(subjectRatings, otherRatings)
  }


  def borne(corr: Double, min: Double = -1., max: Double = 1.) = {
    util.restrict(corr, floor = min, cap = max)
  }
  
  def checkPositive(corr: Double) = (!positive || corr > 0)
  
}