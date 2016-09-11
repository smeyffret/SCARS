package fr.cnrs.liris.scars.scorer

object ScorerCounter {
  private var counter = 0
  def count = counter
  def reset = counter = 0
  protected def increment = counter += 1
}

class ScorerCounter(score: Option[Double], count: Int = 1) extends TestScorer(score, count) {
  ScorerCounter.increment
}
