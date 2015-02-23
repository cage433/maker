package maker.utils

import org.scalatest.{Matchers, FreeSpec}
import maker.utils.FileUtils._
import java.util.jar.JarFile
import scala.collection.JavaConversions._

class BuildJarTests extends FreeSpec with Matchers{
  "builds empty jar when given no directories " in {
    FileUtils.withTempDir{
      dir => 
        
        val jarFile = file(dir, "a.jar")
        BuildJar.build(jarFile, Nil)
        new JarFile(jarFile).entries.toList should be ('empty)
    }
  }

  "builds empty jar from empty directory" in {
    FileUtils.withTempDir{
      dir => 
        
        val jarFile = file(dir, "a.jar")
        val subDir = file(dir, "subDir").makeDirs()
        BuildJar.build(jarFile, subDir :: Nil)
        new JarFile(jarFile).entries.toList should be ('empty)
    }
  }

  "Builds jar with files in it" in {
    FileUtils.withTempDir{
      dir => 
        val jarFile = file(dir, "a.jar")
        val subDir = file(dir, "subDir").makeDirs()
        writeToFile(file(subDir, "a_file"), "some text")
        writeToFile(file(subDir, "sub_sub", "another_file"), "some more text")
        BuildJar.build(jarFile, subDir :: Nil)
        val entryNames = new JarFile(jarFile).entries.toList.map(_.getName)
        entryNames should contain ("a_file")
        entryNames should contain ("sub_sub/another_file")
    }
  }
}
