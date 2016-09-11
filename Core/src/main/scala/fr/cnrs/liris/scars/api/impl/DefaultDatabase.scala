package fr.cnrs.liris.scars.api.impl

import fr.cnrs.liris.scars.api._

class DefaultDatabase(
  val ratingRange: Seq[Double],
  val actors: Iterable[Actor],
  val items: Iterable[Item],
  val reviews: Iterable[Review],
  val evalReviews: Iterable[Review],
  val isLeaveOneOut: Boolean) extends Database
