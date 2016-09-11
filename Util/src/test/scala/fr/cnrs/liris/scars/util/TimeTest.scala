package fr.cnrs.liris.scars.util

import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSuite
import org.junit.runner.RunWith

/**
 * @author  Simon Meyffret
 * @version 0.0.1
 * @since   scala 2.8
 * Date: 3 fev. 2011
 * Time: 13:06:02
 */

@RunWith(classOf[JUnitRunner])
class TimeTest extends FunSuite with ShouldMatchers {

  val values = List(1, 42, 2.34, 1039458, 1111111111, 1234567890, System.currentTimeMillis)
  import Time._
  val units = List(MILLI_SECOND, SECOND, MINUTE, HOUR, DAY, WEEK, MONTH, YEAR)
  
  values foreach { value =>
    units foreach { unit =>
      convertAndBack(value, unit)
    }
  }
  
  def convertAndBack(value: Double, unit: Time.Unit) =
  test("%f milli second should be convert one way and the other way with unit %s".format(value, unit)) {
    unit.toMillis(unit.fromMillis(value)) should be (value plusOrMinus(0.001))
    unit.fromMillis(unit.toMillis(value)) should be (value plusOrMinus(0.001))
  }
  
  test("3600 seconds should be 60 minute and 1 hour") {
    val time = Time.SECOND.toMillis(3600)
    Time.MINUTE(time) should be (60)
    Time.HOUR(time) should be (1)
  }
  
}
