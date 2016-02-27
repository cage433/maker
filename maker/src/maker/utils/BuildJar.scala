package maker.utils

import java.io.{File, FileInputStream, FileOutputStream}
import java.util.jar.{JarEntry, JarOutputStream}
import java.util.zip.ZipEntry
import maker.utils.FileUtils._

object BuildJar{

  type ErrorMessage = String

  def build(jarFile : File, directories : Seq[File], fileFilter: File => Boolean = {f: File => true}) : Either[ErrorMessage, Unit] = {
    val BUFFER_SIZE=1000 * 10
    jarFile.delete
    val jarOutputStream = new JarOutputStream(new FileOutputStream(jarFile))
    try {
      directories.filter(_.exists).foreach{
        dir => 
          FileUtils.allProperFiles(dir).filter(fileFilter).foreach{
            file => 
              val fis = new FileInputStream(file)
              try {
                val buffer = Array.fill[Byte](BUFFER_SIZE)(0)
                val entry = new JarEntry(file.relativeTo(dir).getPath)
                jarOutputStream.putNextEntry(entry)

                var endOfFile = false
                while (!endOfFile){
                  val bytesRead = fis.read(buffer, 0, BUFFER_SIZE)
                  if (bytesRead == -1)
                    endOfFile = true
                  else
                    jarOutputStream.write(buffer, 0, bytesRead)
                }
                jarOutputStream.closeEntry
      
              } finally {
                fis.close
              }
          }
      }
      Right(())
    } catch {
      case e : Exception => Left(e.getMessage)
    } finally {
      jarOutputStream.close
    }
  }
}
