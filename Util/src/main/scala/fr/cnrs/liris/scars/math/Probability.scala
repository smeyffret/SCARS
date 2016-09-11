package fr.cnrs.liris.scars.math


object Probability extends Probability

class Probability {
  

 def selectUniformaly[T, I <% Traversable[T]](list: I, n: Int): I = {
    var size = list.size
    if (size <= n)
      list
    else
      selectUniformaly(list, n, size)
  }

  def selectUniformaly[T, I <% Traversable[T]](list: I, n: Int, size: Int) = {
    val r = new scala.util.Random()
    var toSelect = n
    var sizeLeft = size
    list.filter{ _ =>
      val rand = r.nextInt(sizeLeft)
      sizeLeft -= 1
      if (rand < toSelect) {
        toSelect -= 1
        true
      } else {
        false
      }
    }.asInstanceOf[I]
  }  
  

  def select[T, I <% Traversable[(Double,T)]](list: I, n: Int) = {
    val r = new scala.util.Random()
    list.toSeq.sortBy{ case (weight, _) =>
        r.nextDouble * weight
    }.reverse.take(n)
  }
  
  def selectOne[T, I <% Traversable[(Double,T)]](list: I) = {
    val totalWeight = list.map(_._1).sum
    selectOneW(list, totalWeight)
  }
  
  // context view : http://stackoverflow.com/a/2983376
  // Returning same type : http://stackoverflow.com/a/8236035
  def selectOneW[T, I <% Traversable[(Double,T)]](list: I, totalWeight: Double) = {
    var leftWeight = 0.
    val x = scala.math.random * totalWeight
    
    list.find{ case (weight, _) =>
      leftWeight += weight
      x <= leftWeight
    }
  }
  
  def selectW[T](list: Iterable[(Double,T)], n: Int): Iterable[(Double,T)] = {
    val totalWeight = list.map(_._1).sum
    selectW(list, n, totalWeight)
  }
  
  def selectW[T](list: Iterable[(Double,T)], n: Int, totalWeight: Double) = {
    var r = new scala.util.Random()
    def selectRec(list: Iterable[(Double,T)], result: List[(Double,T)], n: Int, weightLeft: Double): Iterable[(Double,T)] = {
      if (n == 0)
        result
      val x = r.nextDouble * weightLeft
      var weightSeen = 0.
      val selected = list.find{ case elem @ (weight, _)=>
        weightSeen += weight
        x <= weightSeen
      }.get
      selectRec(list.filterNot(selected.equals), selected :: result, n - 1, weightLeft - selected._1)
    }
    selectRec(list.view, Nil, n, totalWeight)
  }
  // voir ici : http://stackoverflow.com/questions/7311453/writing-a-generic-fill-method
  
}

// vim: set ts=2 sw=2 et:
