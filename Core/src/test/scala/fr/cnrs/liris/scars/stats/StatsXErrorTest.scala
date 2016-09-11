package fr.cnrs.liris.scars.stats

import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSuite
import org.junit.runner.RunWith
/**
 * @author  Simon Meyffret
 * @version 0.0.1
 * @since   scala 2.8
 * Date: 12/12/10
 * Time: 19:08
 */

@RunWith(classOf[JUnitRunner])
class StatsXErrorTest extends FunSuite with ShouldMatchers {

  val scores =
               List((1,1,4.0,None), (2,1,3.0,Some(1.0)), (3,2,1.0,Some(2.0)), (4,2,2.0,Some(2.0)), (5,2,2.0,Some(2.0)), (6,2,3.0,Some(2.0)))
  // Erreurs :              None            2                     1                     0                   0                    1
  // Ecart moyenne :         0.5            0.5                   1                     0                   0                    1
  // moyenne des notes item 1 : 3.5
  // moyenne des notes item 2 : 2
  // moyenne des notes à trouver : 2.2
  // moyenne des notes trouvées  : 1.8
  // 5 notes calculées
  // 1 note ignorée
  //
  import Stats._
  val statsB = new StatsBuilder(scores, Nil, mae, rmse, wae, rwse)
  val stats = statsB.build()

  test("Computed scores should yield a MAE") {
    stats.get(mae).get should be (4.0/5.0)
  }

  test("Computed scores should yield a RMSE") {
    stats.get(rmse).get should be (math.sqrt(6.0/5.0))
  }

  test("Computed scores should yield a WAE") {
    stats.get(wae).get should be ((2.0*0.5 + 1.0*1.0 + 1.0*1.0)/5)
  }

  test("Computed scores should yield a RWSE") {
    stats.get(rwse).get should be (math.sqrt((4.0*0.25 + 1.0*1.0 + 1.0*1.0)/5))
  }

}
