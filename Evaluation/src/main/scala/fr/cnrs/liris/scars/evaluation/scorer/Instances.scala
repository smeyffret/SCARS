package fr.cnrs.liris.scars.evaluation.scorer

import fr.cnrs.liris.scars.api._
import fr.cnrs.liris.scars.util.Time
import fr.cnrs.liris.scars.evaluation.app._
import fr.cnrs.liris.scars.scorer.Builder._
import fr.cnrs.liris.scars.scorer.util._
import fr.cnrs.liris.scars.scorer.confidence.score._
import fr.cnrs.liris.scars.scorer.confidence.{Confidence => ConfidenceFactory}
import fr.cnrs.liris.scars.scorer.global._
import fr.cnrs.liris.scars.scorer.soa._
import fr.cnrs.liris.scars.scorer.soa.feature._
import fr.cnrs.liris.scars.scorer.social._
import fr.cnrs.liris.scars.scorer.social.impl._
import fr.cnrs.liris.scars.scorer.social.feature._
import fr.cnrs.liris.scars.scorer.social.heuristics._
import fr.cnrs.liris.scars.scorer.social.parent._

object Instances {

  class CoTCoDepth(hops: Int, depth: Int, alpha: Double = 0.5) extends RecSocialScorer(hops, depth, alpha) with SingleParent with CoTCoD

  
  trait NoWeight extends Confidence.Factory {
    override def score() = super.score() match {
      case c: ConfidenceAggregation => c.slice()(2)
      case c => c
    }
  }
  trait NoFresh extends Confidence.Factory {
    override def rating() = ConfidenceFactory.none()
    override def toString() = "nofresh_" + super.toString
  }
  trait NoSize extends Confidence.Factory {
    override def score() = super.score() match {
      case c: ConfidenceAggregation => c.toRight()
      case c: SizeConfidence => ConfidenceFactory.none()
      case c => c
    }
    override def toString() = "nosize_" + super.toString
  }
  trait NoBoth extends NoFresh with NoSize {
    override def toString() = "noboth_" + super.toString.replace("nosize_nofresh_", "")
  }
  
  trait TWEBConfidence extends Confidence.Factory {
    override def rating() = ConfidenceFactory.none()
    override def score() = new ConfidenceAggregation()(
      new RecommenderConfidence(),
      new WeightedVarianceConfidence()
    )
  }
  
  
  def apply(database: Database, settings: Settings): Iterable[Scorer] = {
    implicit val _settings = settings.delta
    
    ConfidenceFactory.freshness_today = ConfidenceFactory.freshness_today_appolicious
    //ConfidenceFactory.freshness_scale = Time.YEAR
    //ConfidenceFactory.freshness_lambda = 100
    //ConfidenceFactory.size_offset = 100
    
    val n = 2
    val ns = (2 to n)
    //return Experiences.sotaConfidence(3).slice(1,2)
    //return Experiences.testConfidence(n)
    //return ns.flatMap(xp) ++ Experiences.globalConfidence(false)
//    return new MoleTrust(n) with Confidence with NoWeight with NoSize :: Nil
    //return Experiences.globalConfidence(false) ++ (
    //return Experiences.raters(1, 3) :: Nil
    //return Experiences.todepth(2){ k => Experiences.raters(1, k) :: Nil } ++ (new GlobalMean() :: Nil)
    //return new RandomWalk(n) :: Nil
    return new TrustWalker(n){
        override val MaxTry = 100
        override val Epsilon = 0.01
      } :: Nil
    return new CoTCoDepth(1, n) with TWEBConfidence with CachedCorrelation with DefaultSimilarItemMean ::
            new CoTCoDepth(1, n) with TWEBConfidence with CachedCorrelation with DefaultActorMean with DefaultSimilarItemMean ::
            new RandomWalk(n) ::
      Nil
    
    def xp(n: Int): Iterable[Scorer] = {
      Experiences.CoTCoDConfidence(n).slice(0,1) ++ Experiences.sotaConfidence(n).slice(0,1)
    }
    (1 to n).map{k => CCoTCoD(k,k)} ++
    List(
      CCoTCoDa(n,n),
      CCoTCoDi(n,n),
      CCoTCoDia(n,n)

//      new MoleTrust(n),
//      new TrustWalker(n) {
//        override val MaxTry = 100
//        override val Epsilon = 0.1
//      }
    )
  }
  
}
