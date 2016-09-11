package fr.cnrs.liris.scars.api;

/**
 *
 * @author smeyffret
 */

import java.util.Date

abstract class Review {
  def actor: Actor
  def item: Item
  def date: Option[Date]
  def rating: Double
  def confidence: Double

  override def equals(other: Any) = other match {
    case review: Review => actor == review.actor && item == review.item
    case _ => false
  }

  override def hashCode = {
    41 * (41 * (41 + actor.hashCode) + item.hashCode) + rating.hashCode
  }

  override def toString = "%s %s %f".format(actor, item, rating)
}
