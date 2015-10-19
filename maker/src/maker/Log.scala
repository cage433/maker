package maker

import ch.qos.logback.classic.Logger
import org.slf4j.LoggerFactory

trait Log {
  lazy val logger = LoggerFactory.getLogger(getClass)
}

