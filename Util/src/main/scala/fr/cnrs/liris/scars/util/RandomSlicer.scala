package fr.cnrs.liris.scars.util

import scala.collection.GenTraversableLike

object RandomSlicer {
  def apply() = new RandomSlicer()
  def apply(percent: Double) = new RandomSlicer() {
    def slice[T <: GenTraversableLike[_,T]](values: T): T = {
      slice(values, percent)
    }
  }
}

class RandomSlicer {
  
  val random = scala.util.Random

  def slice[T <: GenTraversableLike[_,T]](values: T, percent: Double): T = {
    val size = values.size
    val delta = (size * percent / 100).toInt
    val start = random.nextInt(size - delta)
    values.slice(start, start + delta)
  }
  
}
