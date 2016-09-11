package fr.cnrs.liris.scars.util

trait Threshold {
  
  val MaxTry: Int
  
  /**
   * Call function at maximum MaxTry until stopCondition is 
   * Return the list of success return and the number of call to function
   * The list is in reverse order of call
   */
  def iterate[T](function: => Option[T])(stopCondition: List[T] => Boolean ) = {
    var stop = false
    var nbTry = 0
    var walks = List[T]()
    while(!stop && nbTry < MaxTry) {
      nbTry += 1
      function.foreach{ step =>
        walks = step :: walks
        stop = stopCondition(walks)
      }
    }
    (walks, nbTry)
  }
  
}
