package maker

import ch.qos.logback.classic.Logger
import org.slf4j.LoggerFactory
import org.slf4j.helpers.NOPLogger

trait Log {
  lazy val logger = {
    Log.getLogger()
  }
}

object Log {
  private def getLogger(): org.slf4j.Logger = {
    // Moved to a synchronized block as the LoggerFactory returns NOPLoggers whenever
    // these are constructed in parallel
    this.synchronized {
      def rec(numTries: Int): org.slf4j.Logger = {
        val l = LoggerFactory.getLogger(getClass)
        if (l.isInstanceOf[NOPLogger]) {
          if (numTries >= 5)
            throw new Exception(s"Failed to get logger after $numTries attempts")
          Thread.sleep(100)
          rec(numTries + 1)
        } else {
          l
        }
      }
      rec(0)
    }

  }
}

