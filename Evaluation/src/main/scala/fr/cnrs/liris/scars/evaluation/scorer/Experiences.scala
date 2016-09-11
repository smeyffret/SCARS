package fr.cnrs.liris.scars.evaluation.scorer

import fr.cnrs.liris.scars.api._
import fr.cnrs.liris.scars.evaluation.app._
import fr.cnrs.liris.scars.scorer.Builder._
import fr.cnrs.liris.scars.scorer.util._
import fr.cnrs.liris.scars.scorer.confidence.score._
import fr.cnrs.liris.scars.scorer.global._
import fr.cnrs.liris.scars.scorer.soa._
import fr.cnrs.liris.scars.scorer.soa.feature._
import fr.cnrs.liris.scars.scorer.social._
import fr.cnrs.liris.scars.scorer.social.impl._
import fr.cnrs.liris.scars.scorer.social.feature._
import fr.cnrs.liris.scars.scorer.social.heuristics._
import fr.cnrs.liris.scars.scorer.social.parent._

object Experiences {
  
  def comparison(n: Int = 3) = List(
    CoTCoD(n),
    CoTCoDa(n),
    CoTCoDi(n),
    CoTCoDia(n),
    new MoleTrust(n),
    new TrustWalker(n)// { override val MaxTry = 100; override val Epsilon = 0.01 }
  )

  
  def todepth(nMax: Int = 1, nMin: Int = 1)(scorers: (Int => List[Scorer])) = {
    for {
      n <- (nMin to nMax)
      scorer <- scorers(n)
    } yield scorer
  }

  
  def confidence(n: Int) = List(
    new RecSocialScorer(1, n) with SingleParent with Trust { 
      override def toString = "Default_%s".format(n)
    },
    new RecSocialScorer(1, n) with SingleParent with SocialConfidence { 
      override def toString = "Confidence_%s".format(n)
    }
  )
  
  def CoTCoDConfidence(n: Int) = List(
    new RecSocialScorer(1, n) with SingleParent with CoTCoD with CachedCorrelation {
      override def toString = "confident_CoTCoD%s".format(n)
    },
    new RecSocialScorer(1, n) with SingleParent with DefaultItemMean with DefaultActorMean with CoTCoD with CachedCorrelation {
      override def toString = "confident_CoTCoD%s_ia".format(n)
    }
  )
  
  def globalConfidence(cache: Boolean) = List(
    new GlobalCF(cache) with Confidence,
    new ItemBased(cache) with Confidence
  )
  
  def sotaConfidence(n: Int) = List(
    new MoleTrust(n) with Confidence,
    new TrustWalker(n) {
      override val MaxTry = 100
      override val Epsilon = 0.01
    }
  )
  
  def testConfidence(n: Int) = List(
    /*new RecSocialScorer(1, n) with SingleParent with CoTCoD with Confidence with CachedCorrelation {
      override val scoreConfidence = new ConfidenceAggregation(
        //new SizeConfidence()
      )(
        //new WeightConfidence(), 
        //new RecommenderConfidence()
        new VarianceConfidence()
      )
    }, new RecSocialScorer(1, n) with SingleParent with CoTCoD with Confidence with CachedCorrelation {
      override val scoreConfidence = new ConfidenceAggregation(
        //new SizeConfidence()
      )(
        //new WeightConfidence(), 
        //new RecommenderConfidence()
        new WeightedVarianceConfidence()
      )
    }, new RecSocialScorer(1, n) with SingleParent with CoTCoD with Confidence with CachedCorrelation {
      override val scoreConfidence = new ConfidenceAggregation(
        new SizeConfidence()
      )(
        //new WeightConfidence(), 
        //new RecommenderConfidence()
        new VarianceConfidence()
      )
    }, new RecSocialScorer(1, n) with SingleParent with CoTCoD with Confidence with CachedCorrelation {
      override val scoreConfidence = new ConfidenceAggregation(
        new SizeConfidence()
      )(
        //new WeightConfidence(), 
        new RecommenderConfidence()
        //new VarianceConfidence()
      )
    }, new RecSocialScorer(1, n) with SingleParent with CoTCoD with Confidence with CachedCorrelation {
      override val scoreConfidence = new ConfidenceAggregation(
        new SizeConfidence()
      )(
        new WeightConfidence()
        //new RecommenderConfidence(),
        //new VarianceConfidence()
      )
    }, new RecSocialScorer(1, n) with SingleParent with CoTCoD with Confidence with CachedCorrelation {
      override val scoreConfidence = new ConfidenceAggregation(
        new SizeConfidence()
      )(
        new WeightConfidence(), 
        //new RecommenderConfidence(),
        new VarianceConfidence()
      )
    }, new RecSocialScorer(1, n) with SingleParent with CoTCoD with Confidence with CachedCorrelation {
      override val scoreConfidence = new ConfidenceAggregation(
        new SizeConfidence()
      )(
        //new WeightConfidence(), 
        new RecommenderConfidence(),
        new VarianceConfidence()
      )
    }, new RecSocialScorer(1, n) with SingleParent with CoTCoD with Confidence with CachedCorrelation {
      override val scoreConfidence = new ConfidenceAggregation(
        new SizeConfidence()
      )(
        new WeightConfidence(), 
        new RecommenderConfidence()
        //new VarianceConfidence()
      )
    }, new RecSocialScorer(1, n) with SingleParent with CoTCoD with Confidence with CachedCorrelation {
      override val scoreConfidence = new ConfidenceAggregation(
        //new SizeConfidence()
      )(
        new WeightConfidence(), 
        new RecommenderConfidence(),
        new VarianceConfidence()
      )
    },*/ new RecSocialScorer(1, n) with SingleParent with CoTCoD with Confidence with CachedCorrelation {
      override val scoreConfidence = new ConfidenceAggregation(
        //new SizeConfidence()
      )(
        new WeightConfidence(), 
        new RecommenderConfidence(),
        new VarianceConfidence()
      )
    }
   ) ++ (-1 to 5).map{x => new RecSocialScorer(1, n) with SingleParent with CoTCoD with Confidence with CachedCorrelation {
      override val scoreConfidence = new ConfidenceAggregation(
        new SizeConfidence(x)
      )(
        new WeightConfidence(), 
        new RecommenderConfidence(),
        new WeightedVarianceConfidence()
      )
    }}
  
  
  def delta(_settings: DeltaSettings)(n: Int) = List(
    new RecSocialScorer(1, n) with SingleParent with Trust with Delta { 
      val settings = _settings
      override val minReviewsSize = 1
      override def toString = "delta_%s".format(n)
    }
  )
  
  def parents(n: Int) = List(
    new RecSocialScorer(1, n) with SingleParent with Trust { 
      override def toString = "ParentSingle_%s".format(n)
    },
    new RecSocialScorer(1, n) with Orphan with Trust { 
      override def toString = "ParentOrphan_%s".format(n)
    },
    new RecSocialScorer(1, n) with Ancestors with Trust { 
      override def toString = "ParentAncestors_%s".format(n)
    }
  )
  
  def correlation(n: Int) = List(
    new RecSocialScorer(1, n) with SingleParent with Trust { 
      override def toString = "NoCorrelation_%s".format(n)
    },
    new RecSocialScorer(1, n) with SingleParent with Correlation { 
      override def toString = "Correlation_%s".format(n)
    },
    new RecSocialScorer(1, n) with SingleParent with Correlation with CachedCorrelation { 
      override def toString = "CachedCorrelation_%s".format(n)
    }
  )

  def breadth(n: Int) = List(
    new RecSocialScorer(1,n) with SingleParent with Trust {
      override def toString = "NoBreadth_%d".format(n)
    },
    new RecSocialScorer(1,n) with SingleParent with Trust with BreadthPropagation {
      override def toString = "Breadth_%d".format(n)
    }
  )
  
  def experts(n: Int) = List(
    tcod(n),
    expert(n),
    glob_expert(n)
  )
  
  
  def hops(n: Int) = for {
    alpha <- (0.1 to 0.9 by 0.2) :+ 1.
    h <- (2 to n)
  } yield tcod(n,h,alpha)
  
  
  
  def random_friends(n: Int) = for {
    p <- (1 to 7 by 2).toList ++ (10 to 20 by 5) :+ 30
  } yield sample(p,n)
  
  def random_raters(n: Int) = for {
    p <- (1 to 5 by 2).toList ++ (10 to 20 by 10)
  } yield raters(p,n)
  
  def weight_raters(n: Int) = for {
    p <- (1 to 5 by 2).toList ++ (10 to 20 by 10)
  } yield weight(p,n)
  

  
  def sample(p: Int, n: Int) = {
    new RecSocialScorer(1, n) with SingleParent with CoTCoD with CachedCorrelation with FriendsSample {
      val friends_max = p  
      override def toString = "CoTCoD%s_max%s".format(n, p)
    }
  }

  def raters(p: Int, n: Int) = {
    new RecSocialScorer(1, n) with SingleParent with CoTCoD with CachedCorrelation with RatersSample {
      val raters_max = p  
      override def toString = "CoTCoD%s_rmax%s".format(n, p)
    }
  }

  def weight(p: Int, n: Int) = {
    new RecSocialScorer(1, n) with SingleParent with CoTCoD with CachedCorrelation with RatersSample with WeightSample {
      val raters_max = p  
      override def toString = "CoTCoD%s_rwmax%s".format(n, p)
    }
  }

  
  
  def expert(n: Int) = {
    new RecSocialScorer(1, n) with SingleParent with CoTCoD with CachedCorrelation with Expertise {
      override def toString = "CoTCoD%s_xp".format(n)
    }
  }

  def glob_expert(n: Int) = {
    new RecSocialScorer(1, n) with SingleParent with CoTCoD with CachedCorrelation with GlobalExpertise {
      override def toString = "CoTCoD%s_gxp".format(n)
    }
  }

  
  
  def cotcod(n: Int) = {
    new RecSocialScorer(1, n) with SingleParent with CoTCoD with CachedCorrelation {
      override def toString = "CoTCoD%s".format(n)
    }
  }

  def tcod(n: Int, h: Int = 1, alpha: Double = 0.5) = {
    new RecSocialScorer(hops = h, depth = n, alpha = alpha) with SingleParent with Trust with Confidence {
      override def toString = "TCoD%s_%s_%f".format(h,n,alpha)
    }
  }

}
