package maker.utils

import ch.qos.logback.classic.Logger
import scala.language.implicitConversions

object RichLogger{

  implicit def toRichLogger(logger : Logger) = new RichLogger(logger)
  class RichLogger(logger : Logger){
    def infoWithTime[T](message:String)(f: =>T) = {
      val stopwatch = new Stopwatch()
      val oldThreadName = Thread.currentThread.getName
      try {
        Thread.currentThread.setName(oldThreadName + " > " + message)
        logger.info(" Start")
        val result = f;
        logger.info(" Complete. Time: " + stopwatch)
        result
      }
      finally {
        Thread.currentThread.setName(oldThreadName)
      }
    }
  }
}
