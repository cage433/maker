/*
 * Copyright (c) 2011-2012, Alex McGuire, Louis Botterill
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package maker.maven

import maker.utils._
import maker.utils.FileUtils._
import scala.xml._
import scala.collection.JavaConversions._
import java.io._
import java.net.MalformedURLException
import java.text.ParseException
import org.apache.ivy.plugins.parser.m2.{PomWriterOptions, PomModuleDescriptorWriter}
import org.apache.ivy.Ivy
import org.apache.ivy.plugins.parser.xml.XmlModuleDescriptorParser
import org.apache.ivy.core.module.descriptor.ModuleDescriptor
import org.apache.ivy.core.IvyContext
import org.apache.ivy.plugins.parser.m2.PomWriterOptions.ConfigurationScopeMapping
import maker.MakerProps
import maker.project.{Project, TopLevelProject}
import maker.utils.maven._

case object PomWriter {
  def writePom(props : MakerProps, 
               ivyFile : File,
               ivySettingsFile : File,
               pomFile : File,
               confs : List[String],
               moduleDef : ModuleDef,
               pomTemplateFile : Option[File]) {

    IvyLock.synchronized {
      val log = props.log
      val ivy = Ivy.newInstance
      ivy.configure(ivySettingsFile)
      val moduleVersion = moduleDef.projectDef.moduleLibDef.version
      ivy.setVariable("maker.module.version", moduleVersion)
      ivy.setVariable("maker.module.groupid", moduleDef.projectDef.moduleLibDef.gav.groupId.id)

      // add necessary 'context' variables for an OSS compliant POM
      val contextSettings = IvyContext.getContext().getSettings()
      contextSettings.setVariable("ivy.pom.name", moduleDef.projectDef.moduleLibDef.name, true)
      contextSettings.setVariable("maker.licenses", moduleDef.licenses, true)
      contextSettings.setVariable("maker.scm.url", moduleDef.scmDef.url, true)
      contextSettings.setVariable("maker.scm.connection", moduleDef.scmDef.connection, true)
      contextSettings.setVariable("maker.developers", moduleDef.developers, true)
      val settings = ivy.getSettings
      settings.addAllVariables(System.getProperties)
      log.debug("In writePom using ivy, module version: " + moduleVersion)
      val pomWriterOptions : PomWriterOptions = {
        val csm = new ConfigurationScopeMapping(Map("default" -> "compile"))
        log.debug("***** csm:\n" + csm.getScope(Array("default")).toString)

        val moduleDeps = moduleDef.projectDef.dependencyModules.map(_.toIvyPomWriterExtraDependencies)

        log.debug("deps:\n" + moduleDeps.map(d ⇒ d.getGroup + ":" + d.getArtifact + ":" + d.isOptional.toString).mkString(", "))
        val pwo = ((new PomWriterOptions)
          .setConfs(confs.toArray.map(_.trim))
          .setMapping(csm)
          .setArtifactName(moduleDef.projectDef.moduleLibDef.name)
          .setArtifactPackaging("jar")
          .setDescription(moduleDef.projectDef.description)
          .setExtraDependencies(moduleDeps)
          .setPrintIvyInfo(true))

        log.debug(pomTemplateFile.map(pt => "setting pom template to " + pt.getAbsolutePath).getOrElse("Pom template not specified"))
        pomTemplateFile.map(pt => pwo.setTemplate(pt)).getOrElse(pwo)
      }
      try {
        log.debug("***** pwo confs: " + pomWriterOptions.getConfs.toList.mkString(", "))
        val md: ModuleDescriptor = XmlModuleDescriptorParser.getInstance.parseDescriptor(settings, ivyFile.toURI.toURL, false)
        log.debug("about to exec pomModuleDescriptorWriter")
        PomModuleDescriptorWriter.write(md, pomFile, pomWriterOptions)
      }
      catch {
        case e: MalformedURLException => {
          log.error("unable to convert given ivy file to url: " + ivyFile + ": " + e, e)
        }
        case e: ParseException => {
          log.error(e.getMessage, e)
        }
        case e: Exception => {
          log.error("impossible convert given ivy file to pom file: " + e + " from=" + ivyFile + " to=" + pomFile, e)
        }
      }
    }
  }

  /**
   * builds a top-level reactor pom
   */
  def makeTopLevelPom(
        project : TopLevelProject,
        moduleDef : ModuleDef,
        modules : List[Project],
        version : String,
        props : Map[String, String] = Map()) = {

    val id = project.moduleId
    val repos = moduleDef.repositories.filter(_.url != "") // ivy central has no url, for now ignore or it causes invalid poms

    def mkElem(name : String, elemText : String) = Elem(null, name, Null, TopScope, Text(elemText))
    val moduleProps : List[(String, Map[String, String])] = (project.name, project.readIvyProperties(false)) :: project.allStrictlyUpstreamProjects.map(p => (p.name, p.readIvyProperties(true)))
    val propertiesXml = moduleProps.map(p =>
      Comment("unique props extracted from module: '" + p._1 + "'") ++ p._2.map{ case (k, v) => mkElem(k, v) }).flatten.distinct

    <project xsi:schemaLocation='http://maven.apache.org/POM/4.0.0 http://maven.apache.org/maven-v4_0_0.xsd' xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance' xmlns='http://maven.apache.org/POM/4.0.0'>
      <modelVersion>4.0.0</modelVersion>
      <groupId>{id.groupId.id}</groupId>
      <artifactId>{id.artifactId.id}</artifactId>
      <version>{version}</version>
      <packaging>pom</packaging>
      <name>{project.name}</name>

      <properties>
        {propertiesXml}
      </properties>

      <repositories>{
        repos.map(r =>
          <repository>
            <id>{r.id}</id>
            <name>{r.name}</name>
            <url>{r.url}</url>
          </repository>)
        }
      </repositories>

      <profiles>
        <profile>
          <id>maker</id>
          <activation>
            <activeByDefault>true</activeByDefault>
          </activation>
          <modules>{
            modules.map(m =>
              <module>
                {m.rootAbsoluteFile.relativeTo(project.rootAbsoluteFile)}/pom.xml
              </module>)
            }
          </modules>
        </profile>
      </profiles>
    </project>
  }

  // should be configurable and templated etc, but for now this is needed to get going
  def mkBuildPluginDef(project : Project) = {
    def relToRootPath(f : File) = f.relativeTo(project.rootAbsoluteFile)
    <build>
      <sourceDirectory>{relToRootPath(project.layout.sourceDirs.head)}</sourceDirectory>
      <testSourceDirectory>{relToRootPath(project.layout.testSourceDirs.head)}</testSourceDirectory>
      <resources>
        {project.layout.resourceDirs.map(r => <resource><directory>{relToRootPath(r)}</directory></resource>)}
      </resources>
      <plugins>
        <plugin>
          <groupId>net.alchim31.maven</groupId>
          <artifactId>scala-maven-plugin</artifactId>
          <version>3.1.0</version>
          <executions>
            <execution>
              <goals>
                <goal>compile</goal>
                <goal>testCompile</goal>
              </goals>
              <configuration>
                <args>
                  <arg>-make:transitive</arg>
                  <arg>-dependencyfile</arg>
                  <arg>{Text("${project.build.directory}/.scala_dependencies")}</arg>
                </args>
              </configuration>
            </execution>
          </executions>
        </plugin>
        <plugin>
          <groupId>org.apache.maven.plugins</groupId>
          <artifactId>maven-surefire-plugin</artifactId>
          <version>2.6</version>
          <configuration>
            <useFile>false</useFile>
            <disableXmlReport>true</disableXmlReport>
            <!-- If you have classpath issue like NoDefClassError,... -->
            <!-- useManifestOnlyJar>false</useManifestOnlyJar -->
            <includes>
              <include>**/*Test.*</include>
              <include>**/*Suite.*</include>
            </includes>
          </configuration>
        </plugin>
        <plugin>
          <groupId>org.apache.maven.plugins</groupId>
          <artifactId>maven-jar-plugin</artifactId>
          <version>2.2</version>
          <executions>
            <execution>
              <goals>
                <goal>test-jar</goal>
              </goals>
            </execution>
          </executions>
        </plugin>
      </plugins>
    </build>
  }

  /**
   * make a regular pom for a single module
   */
  def makePom(
        moduleDef : ModuleDef,
        project : Project,
        maybeParentProject : Option[TopLevelProject],
        includeRepos : Boolean = false,
        includePlugin : Boolean = true) = {

    def mkDependencies(dependencies : List[DependencyLib]) : NodeSeq = {
      <dependencies>{
      dependencies.map(d => {
        val gav = d.gav
        <dependency>
          <groupId>{gav.groupId.id}</groupId>
          <artifactId>{gav.artifactId.id}</artifactId>
          {gav.version.map(v => <version>{v.version}</version>).toList}
          <scope>{d.scope.mavenName}</scope>
          {d.maybeType.map(t => <type>{t.mavenName}</type>).toList}
          {d.maybeClassifier.map(c => <classifier>{c.mavenName}</classifier>).toList}
          {if (!project.dependencyAdjustments.additionalExcludedLibs.isEmpty)
            <exclusions>
              {project.dependencyAdjustments.additionalExcludedLibs.map(e => {
              <exclusion>
                <groupId>{e.groupId.id}</groupId>
                <artifactId>{e.artifactId.id}</artifactId>
              </exclusion>
              })}
            </exclusions>
          }
        </dependency>})}
      </dependencies>
    }

    def mkRepositories(repositories : List[MavenRepository]) : NodeSeq = {
      if (includeRepos) {
        <repositories>{
        repositories.map(r =>
          <repository>
            <id>{r.id}</id>
            <name>{r.name}</name>
            <url>{r.url}</url>
            <layout>{r.layout}</layout>
          </repository>)}
        </repositories>
      }
      else Nil
    }

    val parentRef : NodeSeq = {
      maybeParentProject.map(tlp => {
        val tmlGav = tlp.moduleId
        val relativePom = file(tlp.rootAbsoluteFile.relativeTo(project.rootAbsoluteFile), "pom.xml")

        <parent>
          <groupId>{tmlGav.groupId.id}</groupId>
          <artifactId>{tmlGav.artifactId.id}</artifactId>
          <version>{moduleDef.projectDef.moduleLibDef.version}</version>
          <relativePath>{relativePom}</relativePath>
        </parent>
      }).toList
    }

    val gav = moduleDef.projectDef.moduleLibDef.gav

    <project xsi:schemaLocation="http://maven.apache.org/POM/4.0.0 http://maven.apache.org/xsd/maven-4.0.0.xsd" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns="http://maven.apache.org/POM/4.0.0">
      <!--
      Apache Maven 3.x POM generated by Maker
      http://github.com/cage433/maker
      -->
      <modelVersion>4.0.0</modelVersion>
      <groupId>{gav.groupId.id}</groupId>
      <artifactId>{gav.artifactId.id}</artifactId>
      <packaging>jar</packaging>
      <description>{moduleDef.projectDef.description}</description>
      <version>{moduleDef.projectDef.moduleLibDef.version}</version>
      <name>{moduleDef.projectDef.moduleLibDef.name}</name>
      <organization>
        <name>{project.props.Organisation()}</name>
      </organization>
      {parentRef}
      {project.props.PomPluginRepo()}
      {if (includePlugin) mkBuildPluginDef(project) else Nil}
      {mkDependencies(moduleDef.dependencies ::: moduleDef.projectDef.dependencyModules)}
      {mkRepositories(moduleDef.repositories)}
    </project>
  }

  /// write pretty xml to a file, includes xml doc-type
  def writeXmlToFile(file : File, xml : Elem) = {
    val xmlStr = new PrettyPrinter(160, 2).format(xml)
    writeToFile(file, "<?xml version='1.0' encoding='UTF-8'?>\n" + "\n" + xmlStr)
  }
}
