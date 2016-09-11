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
class RandomSliceTest extends FunSuite with ShouldMatchers {

  val list = (1 to 10).toList
  val set = list.toSet
  val map = list.map{x => x -> 2 * x}.toMap
  
  
  test("slicing 10% of a 10 elements list should return a singleton list") {
    val slicer = RandomSlicer(10)
    val slice = slicer.slice(list)
    slice should have size(1)
    slice.toList should be(slice)
    slice.toSet should not be(slice)
    slice intersect list should be(slice)
  }

  test("slicing 50% of a 10 elements list should return a slice with 5 elements") {
    val slicer = RandomSlicer(50)
    val slice = slicer.slice(list)
    slice should have size(5)
    list.indexOfSlice(slice) should be >=(0)
  }

  
  test("slicing 10% of a 10 elements set should return a singleton set") {
    val slicer = RandomSlicer(10)
    val slice = slicer.slice(set)
    slice should have size(1)
    slice.toSet should be(slice)
    slice.toList should not be(slice)
    slice & set should be(slice)
  }

  test("slicing 50% of a 10 elements set should return a slice with 5 elements") {
    val slicer = RandomSlicer(50)
    val slice = slicer.slice(set)
    slice should have size(5)
    slice.subsetOf(set) should be(true)
  }

  
  test("slicing 10% of a 10 elements map should return a singleton map") {
    val slicer = RandomSlicer(10)
    val slice = slicer.slice(map)
    slice should have size(1)
    slice.toMap should be(slice)
    slice.toSet should not be(slice)
    slice.toList should not be(slice)
    slice.keySet & map.keySet should be(slice.keySet)
  }

  test("slicing 50% of a 10 elements map should return a slice with 5 elements") {
    val slicer = RandomSlicer(50)
    val slice = slicer.slice(map)
    slice should have size(5)
    slice.keySet.subsetOf(map.keySet) should be(true)
  }

}
