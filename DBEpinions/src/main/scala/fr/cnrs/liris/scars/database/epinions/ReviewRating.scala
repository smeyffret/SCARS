package fr.cnrs.liris.scars.database.epinions

/**
 *
 * @author smeyffret
 */

object ReviewRating extends Enumeration {
  type Type = ReviewRatingValue
  case class ReviewRatingValue(name: String, weight: Double) extends Val(name)

  val None = ReviewRatingValue("Not Yet Rated", 0.2)
  val Show = ReviewRatingValue("Show", 0.4)
  val LessHelpful = ReviewRatingValue("Somewhat Helpful", 0.6)
  val Helpful = ReviewRatingValue("Helpful", 0.8)
  val VeryHelpful = ReviewRatingValue("Very Helpful", 1.0)
}