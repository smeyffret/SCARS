package fr.cnrs.liris.scars.test

import fr.cnrs.liris.scars.api.Score
import fr.cnrs.liris.scars.scorer.confidence.score.RecommenderConfidence
import fr.cnrs.liris.scars.scorer.social._
import fr.cnrs.liris.scars.scorer.social.impl._
import fr.cnrs.liris.scars.scorer.social.feature.BreadthPropagation
import fr.cnrs.liris.scars.scorer.social.feature._
import fr.cnrs.liris.scars.scorer.util._
import fr.cnrs.liris.scars.scorer.social.parent.SingleParent

/**
 * @author  Simon Meyffret
 * @version 0.0.1
 * @since   scala 2.8
 * Date: 31 janv. 2011
 * Time: 16:55:32
 */


object Conversion{

  implicit def toRating(value: Double, confidence: Double = 1.0, count: Int = 1, actors: Int = 1) = Score(value, confidence, count, actors)
  
  def buildBreadthScorer(hops: Int, depth: Int) = new RecSocialScorer(hops, depth) with SingleParent with BreadthPropagation with Trust

  def buildSocialScorer(hops: Int, depth: Int) = new RecSocialScorer(hops, depth) with SingleParent with Trust

  def buildCycleSocialScorer(hops: Int, depth: Int) = new RecSocialScorer(hops, depth) with SingleParent with Trust

  def buildDeltaCycleSocialScorer(hops: Int, depth: Int, minReviews: Int = 1)(implicit _settings: DeltaSettings) = 
    new RecSocialScorer(hops, depth) with SingleParent with Delta with Trust {
        override val minReviewsSize = minReviews
        val settings = _settings
    }

  def buildConfidenceSocialScorer(hops: Int, depth: Int) = 
    new RecSocialScorer(hops, depth) with SingleParent with SocialConfidence {
      override val scoreConfidence = new RecommenderConfidence()
    }

}
