package fr.cnrs.liris.scars.math

import fr.cnrs.liris.scars.api._
/**
 * @author  Simon Meyffret
 * @version 0.0.1
 * @since   scala 2.8
 * Date: 3 fev. 2011
 * Time: 13:06:02
 */

trait Similarity[Subject <: Identifiable] {

  def computeSimilarity(subject: Subject, without: Set[Review] = Set.empty): Set[(Subject, Subject, Double)]

  def commonRaters(subject: Subject, without: Set[Review] = Set.empty): Set[Subject]

  def computeSimilarityToOthers(subject: Subject, others: Set[Subject],
                                without: Set[Review] = Set.empty,
                                default: Option[Double] = None) = {
    for {
      other <- others
      sim <- similarity(subject, other, without, default)
    } yield (subject, other, sim)
  }

  def similarity(subject: Subject, other: Subject,
                  without: Set[Review] = Set.empty,
                  default: Option[Double] = None): Option[Double]
  
}