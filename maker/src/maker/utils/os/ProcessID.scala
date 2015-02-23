package maker.utils.os

import java.lang.management.ManagementFactory
import maker.MakerProps

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
