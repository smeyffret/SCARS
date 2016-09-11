package fr.cnrs.liris.scars.scorer

import fr.cnrs.liris.scars.api._
import fr.cnrs.liris.scars.scorer.social._
import fr.cnrs.liris.scars.scorer.social.feature._
import fr.cnrs.liris.scars.scorer.social.heuristics._
import fr.cnrs.liris.scars.scorer.social.impl._
import fr.cnrs.liris.scars.scorer.global._
import fr.cnrs.liris.scars.scorer.util._
import social.parent.SingleParent

/**
 * This object is used to build properly scorers
 *
 * @author  Simon Meyffret
 * @version 0.0.1
 * @since   scala 2.8
 * Date: 29 nov. 2010
 * Time: 22:42:52
 */

object Builder {
  
  implicit val defaultSettings = DeltaSettings(0, 1, Some(0.5))

  object DeltaSocialScorer {
    def apply(hops: Int, n: Int, alpha: Double = 0.5, minReviews: Int = 1)(implicit _settings: DeltaSettings) = {
      import _settings._
      require(min < max)
      new RecSocialScorer(hops, n, alpha) with Delta with SingleParent with Trust {
        val settings = _settings
        override val minReviewsSize = minReviews
        override def toString = "RelScorer" + n
      }
    }
  }

  object DeltaCorrSocialScorer {
    def apply(hops: Int, n: Int, alpha: Double = 0.5, sim: Double = 0.5, minReviews: Int = 1)(implicit _settings: DeltaSettings) = {
      import _settings._
      require(min < max)
      new RecSocialScorer(hops, n, alpha) with Delta with Correlation with SingleParent {
        val settings = _settings
        override val minReviewsSize = minReviews
        override val defaultSimilarity = Some(sim)
        override def toString = "CorrRelScorer" + n
      }
    }
  }

  object DeltaConfidentSocialScorer {
    def apply(hops: Int, n: Int, alpha: Double = 0.5, minReviews: Int = 1)(implicit _settings: DeltaSettings) = {
      import _settings._
      require(min < max)
      new RecSocialScorer(hops, n, alpha) with Delta with SocialConfidence with SingleParent {
        val settings = _settings
        override val minReviewsSize = minReviews
      }
    }
  }

  object CoTCoD {
    def apply(n: Int): Scorer = apply(1, n)
    def apply(hops: Int, n: Int, alpha: Double = 0.5) = {
      new RecSocialScorer(hops, n, alpha) with SingleParent with CoTCoD {
        override def toString = "CoTCoD" + n
      }
    }
  }

  object CCoTCoD {
    def apply(n: Int): Scorer = apply(1, n)
    def apply(hops: Int, n: Int, alpha: Double = 0.5) = {
      new RecSocialScorer(hops, n, alpha) with SingleParent with CoTCoD with CachedCorrelation {
        override def toString = "CoTCoD" + n
      }
    }
  }

  object CoTCoDa {
    def apply(n: Int): Scorer = apply(1, n)
    def apply(hops: Int, n: Int, alpha: Double = 0.5, proba: Double = 0.02) = {
      new RecSocialScorer(hops, n, alpha) with SingleParent with DefaultActorMean with CoTCoD {
        override protected val ProbaRating = proba
        override def toString = "CoTCoDa" + n
      }
    }
  }

  object CCoTCoDa {
    def apply(n: Int): Scorer = apply(1, n)
    def apply(hops: Int, n: Int, alpha: Double = 0.5, proba: Double = 0.02) = {
      new RecSocialScorer(hops, n, alpha) with SingleParent with DefaultActorMean with CoTCoD with CachedCorrelation {
        override protected val ProbaRating = proba
        override def toString = "CoTCoDa" + n
      }
    }
  }

  object CoTCoDi {
    def apply(n: Int): Scorer = apply(1, n)
    def apply(hops: Int, n: Int, alpha: Double = 0.5, proba: Double = 0.02) = {
      new RecSocialScorer(hops, n, alpha) with SingleParent with DefaultItemMean with CoTCoD {
        override protected val ProbaRating = proba
        override def toString = "CoTCoDi" + n
      }
    }
  }

  object CCoTCoDi {
    def apply(n: Int): Scorer = apply(1, n)
    def apply(hops: Int, n: Int, alpha: Double = 0.5, proba: Double = 0.02) = {
      new RecSocialScorer(hops, n, alpha) with SingleParent with DefaultItemMean with CoTCoD with CachedCorrelation {
        override protected val ProbaRating = proba
        override def toString = "CoTCoDi" + n
      }
    }
  }

  object CoTCoDia {
    def apply(n: Int): Scorer = apply(1, n)
    def apply(hops: Int, n: Int, alpha: Double = 0.5, proba: Double = 0.02) = {
      new RecSocialScorer(hops, n, alpha) with SingleParent with DefaultItemMean with DefaultActorMean with CoTCoD {
        override protected val ProbaRating = proba
        override def toString = "CoTCoDia" + n
      }
    }
  }

  object CCoTCoDia {
    def apply(n: Int): Scorer = apply(1, n)
    def apply(hops: Int, n: Int, alpha: Double = 0.5, proba: Double = 0.02) = {
      new RecSocialScorer(hops, n, alpha) with SingleParent with DefaultItemMean with DefaultActorMean with CoTCoD with CachedCorrelation {
        override protected val ProbaRating = proba
        override def toString = "CoTCoDia" + n
      }
    }
  }

  object DeltaCoTCoD {
    def apply(hops: Int, n: Int, alpha: Double = 0.5, sim: Double = 0.5, minReviews: Int = 1)(implicit _settings: DeltaSettings) = {
      import _settings._
      require(min < max)
      new RecSocialScorer(hops, n, alpha) with Delta with CoTCoD with SingleParent {
        val settings = _settings
        override val minReviewsSize = minReviews
        override val defaultSimilarity = Some(sim)
        override def toString = "RelCoTCoD" + n
      }
    }
  }

  object DeltaCoTCoDXP {
    def apply(hops: Int, n: Int, alpha: Double = 0.5, sim: Double = 0.5, minReviews: Int = 1)(implicit _settings: DeltaSettings) = {
      import _settings._
      require(min < max)
      new RecSocialScorer(hops, n, alpha) with Delta with CoTCoD with Expertise with SingleParent {
        val settings = _settings
        override val minReviewsSize = minReviews
        override val defaultSimilarity = Some(sim)
        override def toString = "RelCoTCoDXp" + n
      }
    }
  }

  object DeltaGlobalMean {
    def apply(minReviews: Int = 1)(implicit _settings: DeltaSettings) = {
      import _settings._
      require(min < max)
      new GlobalMean() with Delta {
        val settings = _settings
        override val minReviewsSize = minReviews
//        override def toString = "GlobalMean"
      }
    }
  }

  object DeltaGlobalCF {
    def apply(useCache: Boolean = true, minReviews: Int = 1)(implicit _settings: DeltaSettings) = {
      import _settings._
      require(min < max)
      new GlobalCF(useCache) with Delta {
        val settings = _settings
        override val minReviewsSize = minReviews
//        override def toString = "GlobalCF"
      }
    }
  }

}
