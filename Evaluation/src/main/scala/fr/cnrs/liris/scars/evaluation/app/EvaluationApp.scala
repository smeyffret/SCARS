package fr.cnrs.liris.scars.evaluation.app

import fr.cnrs.liris.scars.stats.Stats
import fr.cnrs.liris.scars.util.Monitoring
import fr.cnrs.liris.scars.evaluation.analyse.Process

object EvaluationApp {
  
  var verbose = 1

  def main(args: Array[String]) {
    buildEvaluation(args).run()
  }

  def buildEvaluation(args: Array[String]) = {
    val params = Parser.parseArguments(args)
    import params._
    verbose = verboseLevel

    log("evaluation %s%s%s%s".format(
      builder.toString,
      if (load.isDefined) " ; load" else "",
      if (save.isDefined) " ; save" else "",
      if (decoration.isDefined) " ; " + decoration.get else "")
    , 2)

    val database = time("database: %d", 2)(builder.build)
    
    process.map{ Process.analyse(database, _) }
    
    val settings = new Settings(database, decoration, withView, metrics)
    new EvaluationRun(settings, load, save)
  }
  
  def log(msg: String, level: Int = 1) {
    if (verbose >= level) {
      println(msg)
    }
  }
  
  def time[T](msg: String, level: Int = 1)(f: => T) = {
    val (duration, result) = Monitoring.time(f)
    log(msg.format(duration), level)
    result
  }
  
}

