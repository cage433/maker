/*
 * Copyright (c) 2011-2012, Alex McGuire, Louis Botterill
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met: 
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer. 
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution. 
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 5 DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package maker.utils

import org.slf4j.LoggerFactory
import org.slf4j.Logger
import maker.MakerProps

object MakerLog{
  def retry[T](times: Int, delay: Int = 0)(f: => T): T = {
    assert(times > 0)

    var exception: Option[Throwable] = None
    var done = false
    var result: Option[T] = None

    for (i <- 1 to times if !done) try {
      result = Some(f)
      exception = None
      done = true
    } catch {
      case t: Throwable => {
        exception = Some(t)
        Thread.sleep(delay)
      }
    }

    if (!done) {
      throw exception.get
    }
    result.get
  }

  def apply() : MakerLog = synchronized {
    def getLogger() = {
      LoggerFactory.getLogger(this.getClass).asInstanceOf[Logger]
    }
    val logger: Logger = try {
      retry(10, delay = 500)(getLogger()) // 5 seconds should be long enough
    } catch {
      case t: Throwable ⇒ {
        System.err.println("COULD NOT CREATE LOGGER, GIVING UP.")
        t.printStackTrace(System.err)
        sys.exit(1)
      }
    }
    MakerLog(logger)
  }

}

case class MakerLog(logger: Logger) {

  //def level = logger.getEffectiveLevel


   def debug(msg: => AnyRef) = logger.debug(msg.toString)

   def debug(msg: => AnyRef, t: => Throwable) = logger.debug(msg.toString, t)

   def error(msg: String) = logger.error(msg.toString)

   def error(msg: String, t: Throwable) = logger.error(msg.toString, t)

   def info(msg: => AnyRef) = logger.info(msg.toString)

   def info(msg: => AnyRef, t: => Throwable) = logger.info(msg.toString, t)

   def warn(msg: => AnyRef, t: => Throwable) = logger.warn(msg.toString, t)

   def warn(msg: => AnyRef) = {
    logger.warn(msg.toString)
  }

  //def name = logger.getName

}
