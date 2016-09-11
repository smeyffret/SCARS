package fr.cnrs.liris.scars.api

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import impl.DefaultCategory

/**
 * @author  Simon Meyffret
 * @version 0.0.1
 * @since   scala 2.8
 * Date: 12 avr. 2011
 * Time: 17:11:43
 */

@RunWith(classOf[JUnitRunner])
class CategoryTest extends FunSuite with ShouldMatchers {
  
  test("Two identical categories should be ancestors") {
    val c1 = new DefaultCategory(1, "Category1")
    val c2 = new DefaultCategory(1, "Category1")
    
    c1.isAncestorOf(c2) should be (true)
  }

  test("Two different categories should not be ancestors") {
    val c1 = new DefaultCategory(1, "Category1")
    val c2 = new DefaultCategory(2, "Category2")
    
    c1.isAncestorOf(c2) should not be (true)
  }

  test("A father and his son categories should be ancestors") {
    val c1 = new DefaultCategory(1, "Category1")
    val c2 = new DefaultCategory(2, "Category2")
    c1 addChild c2
    
    c1.isAncestorOf(c2) should be (true)
    c2.isAncestorOf(c1) should not be (true)
  }

  test("A grandfather and his greatson categories should be ancestors") {
    val c1 = new DefaultCategory(1, "Category1")
    val c2 = new DefaultCategory(2, "Category2")
    val c3 = new DefaultCategory(3, "Category3")
    c1 addChild c2
    c2 addChild c3
    
    c1.isAncestorOf(c2) should be (true)
    c2.isAncestorOf(c3) should be (true)
    c1.isAncestorOf(c3) should be (true)
    c2.isAncestorOf(c1) should not be (true)
    c3.isAncestorOf(c2) should not be (true)
    c3.isAncestorOf(c1) should not be (true)
  }

}