package fr.cnrs.liris.scars.evaluation.view

/**
 * @author  Simon Meyffret
 * @version 0.0.1
 * @since   scala 2.8
 * Date: 20 avr. 2011
 * Time: 13:07:54
 */

import fr.cnrs.liris.scars.api._
import fr.cnrs.liris.scars.math.Math.meanBy
import scala.collection.mutable.ListBuffer
import scala.actors.Actor._

abstract class ViewName[+T]

object ColdStartUser extends ViewName[Actor]
object MediumRater extends ViewName[Actor]
object HeavyRater extends ViewName[Actor]

object NoTruster extends ViewName[Actor]
object ColdTruster extends ViewName[Actor]
object MediumTruster extends ViewName[Actor]
object HeavyTruster extends ViewName[Actor]

object Sheep extends ViewName[Actor]
object GraySheep extends ViewName[Actor]
object BlackSheep extends ViewName[Actor]

object Peculiar extends ViewName[Actor]
  
//  type ViewName = Value
//  val NoTruster, ColdTruster, MediumTruster, HeavyTruster, 
//      ColdStartUser, MediumRater, HeavyRater,
//      Sheep, GraySheep, BlackSheep = Value
//}

object View {

  def empty = new View()

  def apply(database: Database) = {

    val views = collection.mutable.Map[ViewName[_], ListBuffer[_]]()
    def add[T](key: ViewName[T], token: T) = {
        val list = views.getOrElseUpdate(key, new ListBuffer[T]()).asInstanceOf[ListBuffer[T]]
        list += token
    }

    val buffer = actor {
      loop {
        react {
          case (key: ViewName[_], actor) => add(key, actor)
          case None => reply(None); exit()
        }
      }
    }

    database.actors.par foreach { user =>
      val friendsCount = user.friendsCount
      val nbReviews = user.reviewsCount
      val reviews = user.reviews
//      if (friendsCount == 0) {
//        buffer ! (NoTruster, user)
//      } else
      if (nbReviews > 0) {

        if (friendsCount == 0) {
          buffer ! (NoTruster, user)
        } else if (friendsCount <= 4) {
          buffer ! (ColdTruster, user)
        } else if (friendsCount < 10) {
          buffer ! (MediumTruster, user)
        } else {
          buffer ! (HeavyTruster, user)
        }

        if (nbReviews <= 4) {
          buffer ! (ColdStartUser, user)
        } else if (nbReviews < 10) {
          buffer ! (MediumRater, user)
        } else {
          buffer ! (HeavyRater, user)
        }
        
        if (true) {
          val categories = for {
            review <- user.reviews
            category = review.item.category
          } yield category
          val friendsCategories = for {
            friend <- user.friends
            review <- friend.reviews
            category = review.item.category
          } yield category
          if ((categories & friendsCategories).isEmpty) {
            buffer ! (Peculiar, user)
          }
        }
        
        if (false && nbReviews > 4) {

          val distance = reviews.view.flatMap { review =>
            val item = review.item
            val rating = review.rating
            meanBy(item.reviews)(_.rating).map { mean =>
              math.abs(rating - mean)
            }
          }.sum / nbReviews
  
          if (distance <= 0.54) {
            buffer ! (Sheep, user)
          } else if (distance <= 0.77) {
            buffer ! (GraySheep, user)
          } else {
            buffer ! (BlackSheep, user)
          }
        
        }

      }
    }

    buffer !? None

    ViewImpl(views.mapValues(_.toSet).toMap)
  }

}

class View {
  def partition[R,S](scores: Iterable[(Actor, Item, R, S)]): 
    List[(String, Iterable[(Actor, Item, R, S)])] = Nil
}

case class ViewImpl(views: Map[ViewName[_], Set[_]]) extends View {
  
  override def partition[R,S](scores: Iterable[(Actor, Item, R, S)]) = {
    
    def actors = getView[Actor,R,S](scores)((set,t) => set.contains(t._1))_
    def no_actors = getView[Actor,R,S](scores)((set,t) => ! set.contains(t._1))_
    
    List(
      no_actors("withTrust", NoTruster),

      actors("coldStartUsers", ColdStartUser),
      actors("mediumRaters", MediumRater),
      actors("heavyRaters", HeavyRater),

      actors("weaklyConnected", ColdTruster),
      actors("fairlyConnected", MediumTruster),
      actors("highlyConnected", HeavyTruster),

      actors("sheep", Sheep),
      actors("graySheep", GraySheep),
      actors("blackSheep", BlackSheep),
      
      actors("peculiar", Peculiar)
    ).flatten
  }
  
  private def getView[T,R,S](scores: Iterable[(Actor, Item, R, S)])
                    (filter: (Set[T], (Actor, Item, R, S)) => Boolean)
                    (name: String, key: ViewName[T]) = {
    val values = scores.filter { filter(get(key), _) }
    if (values.isEmpty) {
      None
    } else {
      Some(name -> values)
    }
  }
  
  private def get[T](key: ViewName[T]) = {
      views.getOrElse(key, Set.empty).asInstanceOf[Set[T]]
  }

}
