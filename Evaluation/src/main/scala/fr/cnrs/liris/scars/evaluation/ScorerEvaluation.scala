package fr.cnrs.liris.scars.evaluation

import fr.cnrs.liris.scars.api._
import fr.cnrs.liris.scars.util.Monitoring.time
import fr.cnrs.liris.scars.stats.Stats

/**
 * Evaluate a scoring algorithm, given a list of computed and real scores
 *
 * Given a scoring algorithm, this class computes the score for each item
 * and calculates teh Mean Absolute Error and the ratio "calculated scores / total scores"
 *
 * @author  Simon Meyffret
 * @version 0.0.1
 * @since   scala 2.8
 * Date: 19 sept. 2010
 * Time: 18:50:15
 */

class ScorerEvaluation {


  /**
   * Runs a function delivering Stats and shows the stats
   *
   * Times the function and print the stats results given by the function
   *
   * @param scorer the scorer used to compute score (i.e the algorithm)
   * @param f a function returning Stats on the data set used by the scorer
   *          (usually 'scorer.score' for each (user,item)'s score missing)
   */
  def stats(scorer: String, otherReviews: Iterable[Review] = Nil)(f: => Iterable[(Actor, Item, Double, Option[Score])]) = {
    val (duration, result) = time(f)
    val (statsDuration, stats) = time(analyse(result, otherReviews))
    printStats(scorer, duration, statsDuration, stats)
    (scorer, duration, statsDuration, stats)
  }

  /**
   * Print stats associated with a scorer and the time of the scoring
   *
   * @param scorer the scorer used to compute scores (i.e. algorithm)
   * @param duration the duration in second to compute every missing scores (depending the data set)
   * @param stats the stats to print (in order : duration, count, total, percent count/total, MAE), e.g:
   *        "2s - N = 100/1000 (10%) - MAE = 0.8456"
   */
  def printStats(scorer: String, duration: Double, statsDuration: Double, stats: Stats) {
    printf("%.0fs (%.0fs) %-21s %s\n", duration, statsDuration, scorer, stats)
  }

  /**
   * Compute stats with the list of computed scores
   *
   * Given the list of computed and real scores for each user and item used for evaluation, compute the
   * statistics for this scorer.
   *
   * @param values the result list containing computed and real scores
   * @return the stats of the scorer
   */
  def analyse(values: Iterable[(Actor, Item, Double, Option[Score])], otherReviews: Iterable[Review]): Stats = {
    Stats.scores(values, otherReviews.view.map{r => (r.actor, r.item, r.rating)})
  }

}
