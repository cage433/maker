package maker

import maker.utils.RichString._
import maker.project.{BaseProject, Project, Module}
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
  def pomXml(baseProject : BaseProject, version : String, includeUpstreamModules : Boolean) = {
    import baseProject.{name, allStrictlyUpstreamModules}
    val groupId = baseProject.organization.getOrElse(throw new IllegalStateException("Organization not defined"))
    val moduleDependencies = if (includeUpstreamModules) 
      Nil
    else 
      allStrictlyUpstreamModules.sortWith(_.name < _.name).map(
        _.pomDependencyXML(version)
      )

    val externalDependencies = {
      val resources = if (includeUpstreamModules)
        baseProject.allUpstreamModules.flatMap(_.resources)
      else {
        baseProject match {
          case _ : Project => Nil
          case m : Module => m.resources
        }
      }
      baseProject.config.scalaVersion.scalaLibraryResource
      (baseProject.config.scalaVersion.scalaLibraryResource +: resources).distinct.map(
        _.pomDependencyXML
      )
    }

    <project xmlns="http://maven.apache.org/POM/4.0.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/maven-v4_0_0.xsd">
    
      <modelVersion>4.0.0</modelVersion>
      <groupId>{groupId}</groupId>
      <artifactId>{name}</artifactId>
      <packaging>jar</packaging>
      <version>{version}</version>
      {baseProject.extraProjectPomInfo}
      <dependencies>
        {moduleDependencies}
        {externalDependencies}
      </dependencies>
    </project>
  }

  def pomXmlText(baseProject : BaseProject, version : String, includeUpstreamModules : Boolean) = {
    val xmlPrinter = new PrettyPrinter(160, 2)
    "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" + 
      xmlPrinter.format(pomXml(baseProject, version, includeUpstreamModules)) 
  }
}
