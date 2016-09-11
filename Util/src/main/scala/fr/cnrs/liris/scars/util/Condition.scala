package fr.cnrs.liris.scars.util

trait Condition {

  def doIf[R](cond: => Boolean)(f: => R) = {
    if (cond)
      Some(f)
    else
      None
  }
  
}
