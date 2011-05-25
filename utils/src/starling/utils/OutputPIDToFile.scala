package starling.utils


import management.ManagementFactory
import java.io.File

/**
 * Mix this trait into the entry point of any application to write out a pid.txt file containing the PID of the java
 * process created.
 */
trait OutputPIDToFile {
  {
    val processName = ManagementFactory.getRuntimeMXBean.getName
    val pid = processName.subSequence(0, processName.indexOf("@")).toString

    val file = new File("pid.txt")
    if (file.exists) file.delete
    val out = new java.io.FileWriter(file)
    out.write(pid + "\n")
    out.close

    Runtime.getRuntime.addShutdownHook(new Thread() {
      override def run() = if (file.exists) file.delete
    })
  }
}