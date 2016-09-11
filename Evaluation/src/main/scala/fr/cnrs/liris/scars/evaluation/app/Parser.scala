/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package fr.cnrs.liris.scars.evaluation.app

import fr.cnrs.liris.scars.evaluation.dataset._
import fr.cnrs.liris.scars.api.DatabaseBuilder
import fr.cnrs.liris.scars.api.impl.PartialDatabaseBuilder
import fr.cnrs.liris.scars.evaluation.analyse.Process
import fr.cnrs.liris.scars.stats.Stats

case class Parameters(builder: DatabaseBuilder,
                      load: Option[String],
                      save: Option[String],
                      decoration: Option[String], 
                      verboseLevel: Int, 
                      process: Option[Process.Value],
                      withView: Boolean,
                      metrics: List[Stats.Value])

object Parser {
  
  val usage =
    """
    Usage:
      EvaluationApp --epinions dir
      EvaluationApp --flixster dir
      EvaluationApp --synpinions dir
      EvaluationApp --database
      
      OPTIONS:
        --load dir
        --reload
        --save dir
        --view
        --process {%s}
        --metrics [%s] (',' separated multivalues)
    """.format((Process.values - Process.Nothing).mkString("|"),
               (Stats.values -- Stats.defaultMetrics).mkString(","))
  
  type OptionMap = Map[Symbol, Any]
  val processes = Process.values.map(v => v.toString -> v).toMap
  val metrics = Stats.values.map(v => v.toString -> v).toMap

  def parseArguments(args: Array[String]) = {
    convertOptions(parseCommandLine(args))
  }
  def parseCommandLine(args: Array[String]) = {
    if (args.isEmpty) {
      println(usage)
      sys.exit(2)
    }
    val arglist = args.toList
    
    def parse(list: List[String]) = nextOption(Map(), list)
    def nextOption(map: OptionMap, list: List[String]): OptionMap = {
      
      def without(others: Symbol*) = others.forall( symbol => !map.contains(symbol) )
      
      list match {
        case Nil => map

        case "--epinions" :: value :: tail if (without('synpinions, 'flixster, 'database)) =>
          nextOption(map ++ Map('epinions -> value), tail)
        // same with appolicious
        case "--appolicious" :: value :: tail if (without('synpinions, 'flixster, 'database)) =>
          nextOption(map ++ Map('epinions -> value), tail)
        
        case "--flixster" :: value :: tail if (without('synpinions, 'epinions, 'database)) =>
          nextOption(map ++ Map('flixster -> value), tail)
        
        case "--synpinions" :: value :: tail if (without('epinions, 'flixster, 'database)) =>
          nextOption(map ++ Map('synpinions -> value), tail)
        
        case "--database" :: tail if (!map.contains('synpinions) && !map.contains('epinions)) =>
          nextOption(map ++ Map('database -> true), tail)

        case "--percent" :: value :: tail if (validPercent(value)) =>
          nextOption(map ++ Map('percent -> value.toDouble), tail)

        case "--load" :: value :: tail =>
          nextOption(map ++ Map('load -> true) ++ Map('loadPath -> value), tail)
        case "--save" :: value :: tail =>
          nextOption(map ++ Map('save -> true) ++ Map('savePath -> value), tail)
        case "--reload" :: tail =>
          nextOption(map ++ Map('reload -> true), tail)

        case "--segmented" :: tail if (without('decoration)) =>
          nextOption(map ++ Map('decoration -> "segmented"), tail)
        case "--leveled" :: tail if (without('decoration)) =>
          nextOption(map ++ Map('decoration -> "leveled"), tail)

        case "--process" :: value :: tail if (processes.contains(value))=>
          nextOption(map ++ Map('process -> value), tail)

        case "--metrics" :: value :: tail if (validMetrics(value))=>
          nextOption(map ++ Map('metrics -> toMetrics(value)), tail)

        case "--verbose" :: tail =>
          nextOption(map ++ Map('verbose -> 2), tail)

        case "--view" :: tail =>
          nextOption(map ++ Map('view -> true), tail)

        case option :: tail =>
          println("Unknown option " + option)
          println(usage)
          sys.exit(1)
      }
    }

    parse(arglist)
  }
  
  def convertOptions(options: OptionMap) = {

    def getBoolean(key: Symbol) = options.get(key).getOrElse(false).asInstanceOf[Boolean]
    def getInt(key: Symbol) = options.get(key).getOrElse(1).asInstanceOf[Int]
    def getDouble(key: Symbol) = options.get(key).getOrElse(1.).asInstanceOf[Double]
    def getString(key: Symbol) = options.get(key).map(_.toString)
    def getList[T](key: Symbol) = options.get(key).getOrElse(List.empty[T]).asInstanceOf[List[T]]

    val save = getString('savePath)
    val reload = getBoolean('reload) && save.isDefined
    val load = getString('loadPath) orElse (if (reload) save else None)
    val decoration = getString('decoration)
    val verbose = getInt('verbose)
    val process = getString('process).flatMap{ processes.get(_) }
    val withView = getBoolean('view)
    val metrics = getList[Stats.Value]('metrics)

    val impl_builder =
      getString('epinions).map(MassaEvaluation.apply).orElse(
        getString('flixster).map(FlixsterEvaluation.apply).orElse(
          getString('synpinions).map(SynpinionsEvaluation.apply)).orElse(
          getString('database).map(_ => DBEpinionsEvaluation.fromJDBC("127.0.0.1", "epinions", "root", "")))).get
    val builder = if(options.contains('percent)) {
      val percent = getDouble('percent)
      new PartialDatabaseBuilder(impl_builder, percent)
    } else {
      impl_builder
    }
      
    
    Parameters(builder, load, save, decoration, verbose, process, withView, metrics)
  }
  
  private def validPercent(percent: String) = try {
    val p = percent.toDouble
    p >= 0 && p <= 100
  } catch {
    case _ => false
  }
  
  private def validMetrics(value: String) = {
    val newMetrics = metricsStrings(value)
    newMetrics subsetOf metrics.keySet
  }

  private def toMetrics(value: String) = {
    val newMetrics = metricsStrings(value)
    for {
      stringMetric <- newMetrics
      metric <- metrics.get(stringMetric)
    } yield metric
  }.toList
  private def metricsStrings(value: String) = {
    value.split(",").map(_.toLowerCase).toSet
  }
}
