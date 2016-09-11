package fr.cnrs.liris.scars.util

object Time {
  case class Unit(coef: Double) {
    def apply(time: Double) = fromMillis(time)
    def fromMillis(time: Double) = time / coef
    def toMillis(time: Double) = time * coef
    def toUnit(time: Double, unit: Unit) = unit.fromMillis(toMillis(time))
  }
  
  implicit private def toInt(unit: Unit) = unit.coef
  implicit private def toUnit(coef: Double) = Unit(coef)

  val MILLI_SECOND: Unit = 1
  val SECOND: Unit = 1000 * MILLI_SECOND
  val MINUTE: Unit = 60 * SECOND
  val HOUR: Unit = 60 * MINUTE
  val DAY: Unit = 24 * HOUR
  val WEEK: Unit = 7 * DAY
  val MONTH: Unit = 30.5 * DAY
  val YEAR: Unit = 365 * DAY
}
