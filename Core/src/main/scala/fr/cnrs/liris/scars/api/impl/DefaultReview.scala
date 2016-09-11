package fr.cnrs.liris.scars.api.impl

import fr.cnrs.liris.scars.api._
import java.util.Date

class DefaultReview(
  val actor: Actor, 
  val item: Item, 
  val rating: Double, 
  val confidence: Double = 1.,
  val date: Option[Date] = None) extends Review
