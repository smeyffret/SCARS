package fr.cnrs.liris.scars.math

import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import scala.collection.immutable.TreeMap

/**
 * @author  Simon Meyffret
 * @version 0.0.1
 * @since   scala 2.8
 * Date: 3 fev. 2011
 * Time: 13:06:02
 */

@RunWith(classOf[JUnitRunner])
class MathTest extends FunSuite with ShouldMatchers {

  test("Test an empty intersection/union") {
    val m1 = TreeMap(1 -> 'a', 2 -> 'b')
    val m2 = TreeMap(                   3 -> 'c', 4 -> 'd')
    Math.interMaps(m1, m2) should be ('empty)
    Math.unionMaps(m1, m2) should (
      be (List((Some('a'),None),(Some('b'),None),(None,Some('c')),(None,Some('d'))))
    )
  }

  test("Test a full intersection") {
    val m1 = TreeMap(1 -> 'a', 2 -> 'b')
    val m2 = TreeMap(1 -> 'c', 2 -> 'd')
    Math.interMaps(m1, m2) should (
      be (List(('a','c'),('b','d')))
    )
    Math.unionMaps(m1, m2) should (
      be (List((Some('a'),Some('c')),(Some('b'),Some('d'))))
    )
  }

  test("Test a partial intersection") {
    val m1 = TreeMap(1 -> 'a', 2 -> 'b', 3 -> 'c', 4 -> 'd', 5 -> 'e')
    val m2 = TreeMap(          2 -> 'e',           4 -> 'f', 5 -> 'e')
    Math.interMaps(m1, m2).toSet should (
      be (Set(('b','e'),('d','f'),('e','e')))
    )
    Math.unionMaps(m1, m2) should (
      be (List((Some('a'),None),(Some('b'),Some('e')),(Some('c'),None),(Some('d'),Some('f')),(Some('e'),Some('e'))))
    )
  }

  test("Test a constant intersection") {
    val m1 = TreeMap(1 -> 'a', 2 -> 'a', 3 -> 'a', 4 -> 'a', 5 -> 'a')
    val m2 = TreeMap(          2 -> 'a',           4 -> 'a', 5 -> 'a')
    Math.interMaps(m1, m2) should (
      be (List(('a','a'),('a','a'),('a','a')))
    )
    Math.unionMaps(m1, m2) should (
      be (List((Some('a'),None),(Some('a'),Some('a')),(Some('a'),None),(Some('a'),Some('a')),(Some('a'),Some('a'))))
    )
  }

  test("Test the mean / variance / standard deviation computation on empty list") {
    val values = List[Double]()
    Math.mean(values) should be (None)
    Math.variance(values) should be (None)
    Math.standardDeviation(values) should be (None)
  }

  test("Test the standard deviation computation on valid List") {
    val values = List[Double](.5, .7, .6, .1, .8)
    
    val sdev = Math.standardDeviation(values)
    sdev.get should be (.241 plusOrMinus 0.001) // Population standard deviation
//    sdev.get should be (.270 plusOrMinus 0.001) // Standard deviation
    
    val mean = Math.mean(values)
    mean should be (Some(2.7 / 5))
  }

  test("Test the weighted mean / variance computation on empty list") {
    val values = List[(Double, Double)]()
    Math.weighted_mean(values) should be (None)
    Math.weighted_variance(values) should be (None)
  }

  test("Test the weighted mean / variance computation on a zero weights list") {
    val values = List[(Double, Double)](0. -> 1., 0. -> 2., 0. -> 3.)
    Math.weighted_mean(values) should be (None)
    Math.weighted_variance(values) should be (None)
  }

  test("Test the weighted mean / variance computation on one zero weight list") {
    val values = List[(Double, Double)](0. -> 1., 1. -> 2., 1. -> 3.)
    Math.weighted_mean(values) should be (Some(2.5))
    Math.weighted_variance(values) should be (Some(0.25))
  }

  test("Test the weighted mean / variance computation on a uniformly weighted list") {
    val values = List[(Double, Double)](2. -> 1., 2. -> 2., 2. -> 3.)
    Math.weighted_mean(values) should be (Some(2))
    Math.weighted_variance(values).get should be (0.666 plusOrMinus 0.001)
  }

  test("Test the weighted mean / variance computation on a weighted list") {
    val values = List[(Double, Double)](0.5 -> 1., 1. -> 2., 2. -> 3.)
    Math.weighted_mean(values).get should be (2.43 plusOrMinus 0.01)
    Math.weighted_variance(values).get should be (0.53 plusOrMinus 0.01)
  }

}
