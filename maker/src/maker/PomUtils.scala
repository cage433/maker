package maker

import maker.project._
import scala.collection.immutable.Nil
import scala.xml.PrettyPrinter

object PomUtils extends DependencyPimps{
  def dependencyXml(groupId : String, artifactId : String, version : String) = {
    <dependency>
      <groupId>{groupId}</groupId>
      <artifactId>{artifactId}</artifactId>
      <version>{version}</version>
      <scope>compile</scope>
    </dependency>
  }

      //<?xml version="1.0" encoding="UTF-8"?>
  def pomXml(project : Project, version : String, scalaVersion : ScalaVersion) = {
    val groupId = project.organization

    def externalDependencies = {
      val scalaLibraryDep = "org.scala-lang" % s"scala-library" % scalaVersion.versionNo
      val projectDependencies : Seq[RichDependency] = project.upstreamModules.flatMap(_.dependencies)
      (scalaLibraryDep +: projectDependencies).distinct.map(
        _.pomXml(scalaVersion)
      )
    }

    <project xmlns="http://maven.apache.org/POM/4.0.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/maven-v4_0_0.xsd">
      <modelVersion>4.0.0</modelVersion>
      <groupId>{groupId}</groupId>
      <artifactId>{project.artifactId}</artifactId>
      <packaging>jar</packaging>
      <version>{version}</version>
      {project.extraProjectPomInfo}
      <dependencies>
        {externalDependencies}
      </dependencies>
    </project>
  }

  def pomXmlText(project : Project, version : String) = {
    val xmlPrinter = new PrettyPrinter(160, 2)
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" + 
      xmlPrinter.format(pomXml(project, version, project.scalaVersion)) 
  }
}
