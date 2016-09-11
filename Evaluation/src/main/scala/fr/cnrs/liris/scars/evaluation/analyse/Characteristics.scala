package fr.cnrs.liris.scars.evaluation.analyse

import fr.cnrs.liris.scars.api._
import scala.actors.Futures._


class Characteristics(database: Database) {
  
  def show(deep: Int) {
    val before = System.currentTimeMillis
    val nbUsers = database.actors.size.toDouble
    val stats = database.actors.map{user =>
      future {
        val scores = user.reviews.map(_.rating)
        val neighbour = (1 to deep).toList map {n =>
          //TODO: prendre en compte 'n'
          user.friends.size
        }
        (scores, neighbour)
      }
    }.map{_()}
    val middle = System.currentTimeMillis
    println("stats : " + (middle - before))
//    val nbUsers = stats.size.toDouble
    val (scores, neighbourUser) = stats unzip
    val scoresFlatten = scores flatten
    val itemsCount = scoresFlatten.size.toDouble
    val scoresMean = scoresFlatten.reduceLeft(_+_) / itemsCount
    
    val neighbour = neighbourUser.flatten.zipWithIndex.groupBy{ case (elem, i) =>
        i % deep
    }.map{ case (n, l) => ( n+1, l.map(_._1).reduceLeft(_+_) / nbUsers)}.toSeq.sorted
    
    printf("users: %f, items: %f, mean: %f, ", nbUsers, itemsCount / nbUsers, scoresMean)
    neighbour.foreach { case (n,size) =>
      printf("n=%d: %f, ", n, size)
    }
    println()
    println("analyse : " + (System.currentTimeMillis - middle))
  }
  
}
