package maker.task.publish

import maker.utils.Implicits.RichString._
import maker.project.BaseProject
import maker.project.Project
import maker.project.Module
import maker.task.update.Resource

object PomUtils{
  def dependencyXml(groupId : String, artifactId : String, version : String) = {
    """|<dependency>
       |  <groupId>%s</groupId>
       |  <artifactId>%s</artifactId>
       |  <version>%s</version>
       |  <scope>compile</scope>
       |</dependency>""".stripMargin % (groupId, artifactId, version)
  }

  def pomXml(baseProject : BaseProject, version : String) = {
    val props = baseProject.props
    val groupId = props.GroupId()
    val name = baseProject.name
    val b = new StringBuffer
    b.addLine(
      """|<?xml version="1.0" encoding="UTF-8"?>
         |<project xmlns="http://maven.apache.org/POM/4.0.0" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
         |    xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/maven-v4_0_0.xsd">
         |
         |  <modelVersion>4.0.0</modelVersion>
         |  <groupId>%s</groupId>
         |  <artifactId>%s</artifactId>
         |  <packaging>jar</packaging>
         |  <version>%s</version>""".stripMargin % (groupId, name, version))
    b.addLine("  <dependencies>")
    baseProject.allStrictlyUpstreamModules.sortWith(_.name < _.name).foreach{
      m => b.addLine(m.pomDependencyXML(version).indent("    "))
    }
    baseProject match {
      case _ : Project =>
      case m : Module => {
        b.addLine(Resource(m, "org.scala-lang", "scala-library", props.ProjectScalaVersion()).pomDependencyXML.indent("    "))
        m.resources.foreach(
          r => 
            b.addLine(r.pomDependencyXML.indent("    "))
        )
      }
    }
    b.addLine("  </dependencies>")
    b.addLine("</project>")
    b.toString
  }
}
