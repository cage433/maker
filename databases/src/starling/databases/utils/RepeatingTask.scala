package starling.databases.utils

import java.util.{Timer, TimerTask}
import starling.utils.Stopable

/**
 * @param runEvery number of seconds to run after, repeats
 */
abstract class RepeatingTask(runEvery: Int, name: String) extends Stopable {

  def task: Unit

  def schedule {
    val timerTask = new TimerTask {
      def run = {
        task
      }
    }

    val timer = new Timer(name, true)

    timer.schedule(timerTask, runEvery * 1000, runEvery * 1000)
  }

  override def start = { super.start; schedule }
}