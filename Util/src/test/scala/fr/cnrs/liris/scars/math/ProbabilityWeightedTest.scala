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
class ProbabilityWeightedTest extends FunSuite with ShouldMatchers {

  val r = Probability

//TODO: voir pourquoi ce test ne passe pas
//  test("A constant weight collection should return the goog number of random elements") {
//    val NB_TRIES = 100
//    val elements = (1 to 10).toList.map{i => (0.5, i)}
//    val tirages = (1 to NB_TRIES).map{ _ =>
//      val result = r.select(elements, 5).map(_._2)
//      result should have size(5)
//      (result.reduceLeft(_+_), result.size)
//    }
//    val randomSum = tirages.map(_._1).reduceLeft(_+_) / NB_TRIES.toDouble
//    val totalSum = elements.map(_._2).reduceLeft(_+_).toDouble
//    randomSum should be( totalSum / 2 plusOrMinus 3)
//  }
  
//  test("Pick one element in ((1., 'A'), (2., 'B'))") {
//    val NB_TRIES = 10000
//    val elements = List((1., 'A'), (2., 'B'))
//    val tirages = (1 to NB_TRIES).flatMap{ _ =>
//      r.select(elements, 1).map(_._2)
//    }
//    val cardMap = tirages.groupBy(identity).map{ case (i, l) => (i, l.size) }.toMap
//    cardMap('A') should be (NB_TRIES / 3 plusOrMinus 3)
//    cardMap('B') should be (NB_TRIES * 2 / 3 plusOrMinus 100)
//  }
  
  {
    val NB_TRIES = 100000
    val PICK = 1
    val POPULATION = (1 to 5)
    val elements = POPULATION.toList.zipWithIndex.map{case (i, index) => (index + 1., i)}
    val probTotal = POPULATION.reduceLeft(_+_)
    val tirages = (1 to NB_TRIES).map{ _ =>
      val result = r.selectOne(elements).map(_._2)
      result should be ('defined)
      result
    }
    val cardMap = tirages.flatten.groupBy(identity).map{ case (i, l) => (i, l.size) }.toMap
    POPULATION.foreach{ i =>
      val prob = NB_TRIES * PICK * i / probTotal 
      test("%d should appear %d times with %d tries".format(i, prob, NB_TRIES)) {
        cardMap(i) should be( prob plusOrMinus prob / 10)
      }
    }
  }
  
//  testDistribution(new RandomSelector(5))
//  testDistribution(new RandomSelector(9))
//  //test("Random elements should be uniformaly distributed") {
//  def testDistribution(r: RandomSelector){
//    val MAX = 1500
//    val EXPECTED = MAX * r.number / 10
//    val MORE_OR_LESS = EXPECTED / 10
//    var distribution = collection.mutable.Map[Int, Int]()
//    (1 to MAX) foreach { _ =>
//      r.select((1 to 10)) foreach { i =>
//        distribution(i) = distribution.getOrElse(i, 0) + 1
//      }
//    }
//    //println(distribution)
//    distribution.foreach{ case (i,d) =>
//      test("%d should have more or less %d apparition in a %d population".format(i, EXPECTED, MAX)) {
//        d should be (EXPECTED plusOrMinus MORE_OR_LESS)
//      }
//    }
//  }
  
}

// vim: set ts=2 sw=2 et:
