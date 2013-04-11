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

import ch.qos.logback.classic.Level
import ch.qos.logback.classic.Logger
import org.slf4j.LoggerFactory
import maker.MakerProps

object MakerLog{
  def apply(level : Level = Level.INFO, name : String = "STDOUT") : MakerLog = {
    synchronized{
      val logger = LoggerFactory.getLogger(name).asInstanceOf[Logger]
      logger.setLevel(level)
      MakerLog(logger)
    }
  }

}

case class MakerLog(logger: Logger) {

  def level = logger.getEffectiveLevel

  def infoWithTime[T](message:String)(f: =>T) = {
    val stopwatch = new Stopwatch()
    val oldThreadName = Thread.currentThread.getName
    try {
      Thread.currentThread.setName(oldThreadName + " > " + message)
      info(" Start")
      val result = f;
      info(" Complete. Time: " + stopwatch)
      result
    }
    finally {
      Thread.currentThread.setName(oldThreadName)
    }
  }


   def debug(msg: => AnyRef) = logger.debug(msg.toString)

   def debug(msg: => AnyRef, t: => Throwable) = logger.debug(msg.toString, t)

   def error(msg: => AnyRef) = logger.error(msg.toString)

   def error(msg: => AnyRef, t: => Throwable) = logger.error(msg.toString, t)

   def info(msg: => AnyRef) = logger.info(msg.toString)

   def info(msg: => AnyRef, t: => Throwable) = logger.info(msg.toString, t)

   def warn(msg: => AnyRef, t: => Throwable) = logger.warn(msg.toString, t)

   def warn(msg: => AnyRef) = logger.warn(msg.toString)

   def setLevel(level: Level) = {
     logger.setLevel(level)
   }

   def withLevel[T](newLevel: Level)(thunk: => T) = {
    val savedLevel = logger.getEffectiveLevel(); 
    setLevel(newLevel); 
    val thunkVal = {thunk}; 
    setLevel(savedLevel); 
    thunkVal 
  }

   def name = logger.getName

}
