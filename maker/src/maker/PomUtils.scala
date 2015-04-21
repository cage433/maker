package maker

import maker.utils.RichString._
import maker.project._
import scala.collection.immutable.Nil
import scala.xml.PrettyPrinter

object PomUtils extends ConfigPimps{
  def dependencyXml(groupId : String, artifactId : String, version : String) = {
    <dependency>
      <groupId>{groupId}</groupId>
      <artifactId>{artifactId}</artifactId>
      <version>{version}</version>
      <scope>compile</scope>
    </dependency>
  }

      //<?xml version="1.0" encoding="UTF-8"?>
  def pomXml(project : Project, version : String) = {
    import project.name
    val groupId = project.organization.getOrElse(throw new IllegalStateException("Organization not defined"))

    val externalDependencies = {
      val resources = project.upstreamModules.flatMap(_.resources)
      project.config.scalaVersion.scalaLibraryResource
      (project.config.scalaVersion.scalaLibraryResource +: resources).distinct.map(
        _.pomDependencyXML
      )
    }

    <project xmlns="http://maven.apache.org/POM/4.0.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/maven-v4_0_0.xsd">
    
      <modelVersion>4.0.0</modelVersion>
      <groupId>{groupId}</groupId>
      <artifactId>{name}</artifactId>
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
      xmlPrinter.format(pomXml(project, version)) 
  }
}
