package fr.cnrs.liris.scars.evaluation

import fr.cnrs.liris.scars.api.Score
import java.io.FileWriter
import java.util.Locale
import scala.actors.Actor

/**
 * Allow to save data in a file.
 *
 * @author  Simon Meyffret
 * @version 0.0.1
 * @since   scala 2.8
 * Date: 29 oct. 2010
 * Time: 15:42:35
 *
 * @param path the file path containing data, opened at creation
 */

abstract class DataSaver extends Actor {
  override def start() = super.start()
  def close(): Unit = super.exit()
}

/**
 * Default DataSaver : don't save anything
 */
object NoneSaver extends DataSaver {
  def act() = {
    loop {
      react {
        case None => close()
      }
    }
  }
}

class FileDataSaver(path: String) extends DataSaver {


  /**
   * Save data in the file
   * @param user the user who scored the item
   * @param item the item scored
   * @param value to save
   */
  def act() {
    loop {
      react {
        case None => close()
        case (user, item, None) =>
          val line = "%s %s\n".formatLocal(Locale.ENGLISH, user, item)
          write(line)
        case (user, item, Some(Score(value, confidence, count, actors, time))) =>
          val line = "%s %s %s %s %s\n".formatLocal(Locale.ENGLISH, user, item, value, confidence, count)
          write(line)
        case (user, other, correlation) =>
          val line = "%s %s %s\n".formatLocal(Locale.ENGLISH, user, other, correlation)
          write(line)
        case other => write(other.toString + "\n")
      }
    }
  }

  private var fw: FileWriter = _
  
  override def start() = {
    fw = new FileWriter(path)
    super.start()
  }
  
  /**
   * Close the file used to read/write data.
   */
  override def close() {
    fw.close()
    super.close()
  }

  /**
   * Write any line to the file
   */
  def write(line: String) = try {
    fw.write(line)
  } catch {
    case e => println(line); throw e
  }

}
