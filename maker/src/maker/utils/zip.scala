package maker.utils

import java.io.File
import java.io.FileInputStream
import java.io.FileOutputStream
import java.util.zip.ZipInputStream

object Zip {
  def unzip(zipFile: File, folder: File): Unit = {
    val buffer = Array.ofDim[Byte](1024)
    folder.mkdirs()
    val zis = new ZipInputStream(new FileInputStream(zipFile))
    try {
      var ze = zis.getNextEntry()
      while (ze != null) {
        val newFile = new File(folder + "/" + ze.getName())
        if (ze.isDirectory()) newFile.mkdirs()
        else {
          new File(newFile.getParent()).mkdirs()
          val fos = new FileOutputStream(newFile)
          try {
            var len = 0
            do {
              len = zis.read(buffer)
              if (len > 0) fos.write(buffer, 0, len)
            } while (len > 0)

          } finally fos.close()
        }
        ze = zis.getNextEntry()
      }
      zis.closeEntry()
    } finally zis.close()
  }
}
