package fr.cnrs.liris.scars.util

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
class ThresholdTest extends FunSuite with ShouldMatchers {

  val threshold = new Threshold { override val MaxTry = 10 }

  test("Immediate stop condition should return one success step in one try") {
    def condition(l: List[Int]) = true
    def next() = Some(1)
    val (list, nb) = threshold.iterate(next)(condition)
    nb should be (1)
    list should be (List(1))
  }
  
  test("Immediate stop condition should return no success with only None") {
    def condition(l: List[Int]) = true
    def next() = None
    val (list, nb) = threshold.iterate(next)(condition)
    nb should be (threshold.MaxTry)
    list should be (List())
  }
  
  test("Perpetual stop condition should return MaxTry successes in MaxTry tries") {
    def condition(l: List[Int]) = false
    def next() = Some(1)
    val (list, nb) = threshold.iterate(next)(condition)
    nb should be (threshold.MaxTry)
    list should be (List.fill(threshold.MaxTry)(1))
  }
  
  test("Stop condition after n tries should return n successes in n tries") {
    val Count = 5
    val it = List.range(1, threshold.MaxTry + 1).iterator
    def condition(l: List[Int]) = l.size == Count
    val (list, nb) = threshold.iterate(Some(it.next))(condition)
    nb should be (Count)
    list should be (List.range(1, Count + 1).reverse)
  }
  
}
