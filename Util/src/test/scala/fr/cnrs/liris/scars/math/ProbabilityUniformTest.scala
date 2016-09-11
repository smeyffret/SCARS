package fr.cnrs.liris.scars.math

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

/**
 * @author  Simon Meyffret
 * @version 0.0.1
 * @since   scala 2.8
 * Date: 9 déc. 2010
 * Time: 12:32:04
 */

@RunWith(classOf[JUnitRunner])
class ProbabilityUniformTest extends FunSuite with ShouldMatchers {

  val r = Probability
  
  val PICK = 5

  test("A bigger collection should return the goog number of random elements") {
    val NB_TRIES = 100
    val elements = (1 to 10).toList
    val tirages = (1 to NB_TRIES).map{ _ =>
      val result = r.selectUniformaly(elements, PICK)
      result should have size(PICK)
      result.reduceLeft(_+_)
    }
    val randomSum = tirages.reduceLeft(_+_) / NB_TRIES
    val totalSum = elements.reduceLeft(_+_)
    randomSum should be( totalSum / 2 plusOrMinus 3)
  }
  
  testDistribution(5)
  testDistribution(9)
  //test("Random elements should be uniformaly distributed") {
  def testDistribution(pick: Int){
    val MAX = 1500
    val EXPECTED = MAX * pick / 10
    val MORE_OR_LESS = EXPECTED / 10
    val elements = (1 to 10).toList
    var distribution = collection.mutable.Map[Int, Int]()
    (1 to MAX) foreach { _ =>
      r.selectUniformaly(elements, pick) foreach { i =>
        distribution(i) = distribution.getOrElse(i, 0) + 1
      }
    }
    //println(distribution)
    distribution.foreach{ case (i,d) =>
      test("%d should have more or less %d apparition in a %d population".format(i, EXPECTED, MAX)) {
        d should be (EXPECTED plusOrMinus MORE_OR_LESS)
      }
    }
  }
  
}

// vim: set ts=2 sw=2 et:
