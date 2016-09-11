/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.cnrs.liris.scars.util

object Monitoring {

  /**
   * Times the execution of a function, and return the duration with the function result
   *
   * @param f the function to execute
   * @return a tuple containing the execution duration (in second) and the result of the function
   */
  def time[T](f: => T) = {
    val begin = System.currentTimeMillis
    val result: T = f
    val duration = (System.currentTimeMillis - begin) / 1000
    (duration, result)
  }
  
}
