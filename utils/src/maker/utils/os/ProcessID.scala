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
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package maker.utils.os

import java.lang.management.ManagementFactory
import maker.MakerProps
import maker.utils.MakerLog

case class ProcessID(id : Int){
  def isRunning() = {
    val status = Command("kill", "-0", id.toString).withNoOutput.exec()
    status == 0
  }

  def kill(){
    val status = Command("kill", "-9", id.toString).withNoOutput.exec()
    assert(status == 0, "Failed to kill process " + id + ", ")
  }
}

object ProcessID{
  /**
   * Returns the process ID of the current process
   * Not certain this would work any anything other than Linux, but 
   * is only used in tests
   */
  def apply() : ProcessID = {
    val List(idString, host) = ManagementFactory.getRuntimeMXBean().getName().split("@").toList
    ProcessID(idString.toInt)
  }
  def apply(proc : Process) : ProcessID = {
    assert(OsUtils.isUnix, "TODO - find out how to get process ID on a windows box")
    val f = proc.getClass().getDeclaredField("pid")
    f.setAccessible(true)
    ProcessID(f.getInt(proc))
  }
}
