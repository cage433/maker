package maker.project

import maker.utils.FileUtils._
import maker.MakerProps
import java.io.File
import java.lang.reflect.Modifier
import java.net.URLClassLoader
import maker.task.compile._

trait ProjectFilesAndPaths{
  self : Project ⇒ 

  def cacheDirectory = mkdirs(file(rootAbsoluteFile, ".maker", "cache"))

  def classpathJarsOnly : Set[File] = findJars(layout.unmanagedLibDirs ++ layout.providedLibDirs + layout.managedLibDir) + self.props.ScalaLibraryJar()
  def classpathJars: Set[File] = allUpstreamProjects.toSet.flatMap{proj : Project ⇒ proj.classpathJarsOnly}

  def outputArtifact = file(layout.packageDir.getAbsolutePath, name + ".jar")


  lazy val isAccessibleScalaTestSuite : (String ⇒ Boolean) = {
    lazy val loader = new URLClassLoader(
      allUpstreamProjects.flatMap{p ⇒ p.classpathJarsOnly + p.layout.testOutputDir + p.layout.outputDir}.map(_.toURI.toURL).toArray,
      null
    )
    lazy val suiteClass = loader.loadClass("org.scalatest.Suite")
    
    (className: String) ⇒  {
      val suiteClass = loader.loadClass("org.scalatest.Suite")
      val emptyClassArray = new Array[java.lang.Class[T] forSome {type T}](0)
      val clazz = loader.loadClass(className)
      try {
        suiteClass.isAssignableFrom(clazz) &&
          Modifier.isPublic(clazz.getModifiers) &&
          !Modifier.isAbstract(clazz.getModifiers) &&
          Modifier.isPublic(clazz.getConstructor(emptyClassArray: _*).getModifiers)
      }
      catch {
        case _: NoSuchMethodException => false
        case _: SecurityException => false
        case _: ClassNotFoundException => false
        case _: NoClassDefFoundError => false
      }
    }
  }





}
