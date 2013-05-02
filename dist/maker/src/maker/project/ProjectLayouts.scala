package maker.project

import java.io.File
import maker.utils.FileUtils._

/**
 * Defines project layout in terms of directory structures of inputs and outputs
 *   and some default layouts, e.g. maven and maker
 */
trait ProjectLayout extends DelayedInit{
  def sourceDirs : Set[File]
  def testSourceDirs : Set[File]
  def targetDir : File
  def outputDir : File
  def testOutputDir : File
  def resourceDirs : Set[File]
  def testResourceDirs : Set[File] // When running tests we use this AS WELL as resourceDirs
  def docDir : File
  def packageDir : File
  def ivyFile : File
  def ivySettingsFile : File
  def managedLibDir : File
  def managedLibSourceDir : File
  def managedResourceDir : File
  def unmanagedLibDirs : Set[File]
  def providedLibDirs : Set[File]

  def outputDirs = List(outputDir, testOutputDir, docDir, packageDir)
  def delayedInit(body : ⇒ Unit){
    body
    outputDirs.foreach(_.mkdirs)
    outputDirs.foreach {
      dir ⇒
        require(dir.isContainedIn(targetDir), "Output directory " + dir.getAbsolutePath + " not contained in target " + targetDir.getAbsolutePath)
    }
  }

}


class MavenProjectLayout(private val root : File, targetDirName : String = "target") extends ProjectLayout {
  val rootAbsoluteFile = root.asAbsoluteFile
  def targetDir = file(rootAbsoluteFile, targetDirName)
  def sourceDirs = Set(file(rootAbsoluteFile, "src", "main", "scala"), file(rootAbsoluteFile, "src", "main", "java"))

  def testSourceDirs = Set(file(rootAbsoluteFile, "src", "test", "scala"), file(rootAbsoluteFile, "src", "test", "java"))
  def outputDir = file(targetDir, "classes")
  def testOutputDir = file(targetDir, "test-classes")
  def resourceDirs = Set(file(rootAbsoluteFile, "src", "main", "resources"))
  def testResourceDirs = Set(file(rootAbsoluteFile, "src", "test", "resources"))
  def docDir = file(targetDir, "docs")
  def packageDir = file(targetDir, "package")
  def ivyFile = file(rootAbsoluteFile, "ivy.xml")
  def ivySettingsFile = file("ivysettings.xml") // Note that this is relative to CWD
  def managedLibDir = file(rootAbsoluteFile, "lib_managed")
  def managedLibSourceDir = file(rootAbsoluteFile, "lib_src_managed")
  def managedResourceDir = file(rootAbsoluteFile, "resource_managed")
  def unmanagedLibDirs = Set(file(rootAbsoluteFile, "lib"))
  def providedLibDirs = Set.empty // compile time only, don't add to packaging as it is expected that this will be provided by the runtime environment.
}


object MavenProjectLayout{
  def apply(root : File) = new MavenProjectLayout(root)
}

class MakerProjectLayout(private val root : File) extends ProjectLayout{
  val rootAbsoluteFile = root.asAbsoluteFile
  def sourceDirs = Set(file(rootAbsoluteFile, "src"))
  def testSourceDirs = Set(file(rootAbsoluteFile, "tests"))
  def targetDir = file(rootAbsoluteFile, "target-maker")
  def outputDir = file(targetDir, "classes")
  def testOutputDir = file(targetDir, "test-classes")
  def resourceDirs = Set(file(rootAbsoluteFile, "resources"))
  def testResourceDirs = Set(file(rootAbsoluteFile, "test-resources"))
  def docDir = file(targetDir, "docs")
  def packageDir = file(targetDir, "package")
  def ivyFile = file(rootAbsoluteFile, "ivy.xml")
  def ivySettingsFile = file("ivysettings.xml") // Note that this is relative to CWD
  def managedLibDir = file(rootAbsoluteFile, "lib_managed")
  def managedLibSourceDir = file(rootAbsoluteFile, "lib_src_managed")
  def managedResourceDir = file(rootAbsoluteFile, "resource_managed")
  def unmanagedLibDirs = Set(file(rootAbsoluteFile, "lib"))
  def providedLibDirs = Set.empty // compile time only, don't add to packaging as it is expected that this will be provided by the runtime environment.
}

object MakerProjectLayout{
  def apply(root : File) = new MakerProjectLayout(root)
}
