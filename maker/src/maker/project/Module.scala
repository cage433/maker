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
package maker.project

import java.io.File
import maker.utils._
import maker.utils.Utils._
import os.Command
import org.apache.commons.io.FileUtils._
import xml.NodeSeq
import maker.utils.FileUtils._
import scala.tools.nsc.Settings
import scala.tools.nsc.reporters.ConsoleReporter
import java.io.PrintWriter
import scala.tools.nsc.io.PlainDirectory
import scala.tools.nsc.Global
import scala.tools.nsc.io.Directory
import maker.utils.maven._
import maker.MakerProps
import org.apache.commons.io.output.TeeOutputStream
import org.apache.commons.io.output.NullOutputStream
import java.io.PrintStream
import java.io.FileOutputStream
import java.io.PrintStream
import sbt.ConsoleLogger
import com.typesafe.zinc.Setup
import scala.collection.JavaConversions._
import com.typesafe.zinc.Compiler
import maker.task.compile.CompileTask
import maker.task.compile.CompileTask
import maker.task.Dependency
import java.util.concurrent.ConcurrentHashMap
import sbt.inc.Analysis
import maker.task.compile._
import com.typesafe.zinc.ZincClient
import xsbti.compile.CompileOrder
import com.typesafe.zinc.Parsed
import com.typesafe.zinc.Settings
import com.typesafe.zinc.Inputs
import com.typesafe.zinc.Setup
import maker.task.Build
import maker.task.tasks.CleanTask
import maker.task.tasks.RunUnitTestsTask
import maker.task.tasks.UpdateTask
import maker.Resource
import maker.utils.RichString._
import scala.xml.Elem
import maker.ivy.IvyUtils
import maker.PomUtils

/**
  * Corresponds to a module in IntelliJ
  */

class Module(
    protected val root : File,
    val name : String,
    val immediateUpstreamModules : List[Module] = Nil,
    val immediateUpstreamTestModules : List[Module] = Nil,
    val props : MakerProps = MakerProps(),
    val analyses : ConcurrentHashMap[File, Analysis] = Module.analyses
)
  extends BaseProject
  with TmuxIntegration
{

  val modules = List(this)

  val resourcesFile = file(root, "external-resources")

  def resources() : List[Resource]  = {
    
    val resources = resourcesFile.readLines.toList.filterNot{
      line => 
        line.startsWith("#") || line.trim.size == 0
    }.map(Resource.build(this, _, props.resourceVersions(), props.resourceResolvers()))
    val sourceResources = resources.filter(_.extension == "jar").map(_.copy(classifier = Some("sources")))
    (resources ::: sourceResources).distinct
  }
  Module.warnOfUnnecessaryDependencies(this)
  /**
   * The standard equals method was slow, making Dependency operations very expensive.
   */
   override def equals(rhs : Any) = {
     rhs match {
       case p : Module if p.root == root ⇒ {
         //I believe this assertion should always hold. It's really here so that
         //this overriden equals method never returns true on differing modules.
         assert(this eq p, "Shouldn't have two modules pointing to the same root")
         true
       }
       case _ ⇒ false
     }
   }

  override def hashCode = root.hashCode

  private def warnOfRedundantDependencies() {
    immediateUpstreamModules.foreach{
      module ⇒
        val otherUpstreamModules = immediateUpstreamModules.filterNot(_ == module)
        otherUpstreamModules.find(_.allUpstreamModules.contains(module)) match {
          case Some(otherUpstreamModule) ⇒
          log.warn(name + " shouldn't depend on " + module.name + " as it is inherited via " + otherUpstreamModule.name)
          case None ⇒
        }
    }
  }

  warnOfRedundantDependencies()

  def pomDependencyXML(version : String) = PomUtils.dependencyXml(groupId, artifactId, version)
  def testCompilePhase = ModuleCompilePhase(this, TestCompilePhase)
  def compilePhase = ModuleCompilePhase(this, SourceCompilePhase)


  lazy val allUpstreamModules         : List[Module] = this :: allStrictlyUpstreamModules
  lazy val allUpstreamTestModules         : List[Module] = this :: allStrictlyUpstreamTestModules
  private lazy val allStrictlyUpstreamTestModules : List[Module] = immediateUpstreamTestModules.flatMap(_.allUpstreamTestModules).distinct.sortWith(_.name < _.name)

  override def toString = name

  /**************************
  *       TASKS
  **************************/

  lazy val CleanOnly = Build(
    "Clean only " + name, 
    () ⇒ Dependency.Graph(CleanTask(this)), 
    this,
    "cleanOnly",
    "Deletes classes (source and test) for module " + name + ", leaving upstream module untouched"
  )

  lazy val TestOnly = Build(
    "Test " + name + " only", 
    () ⇒ Dependency.Graph.transitiveClosure(this, RunUnitTestsTask(this)), 
    this,
    "testOnly",
    "Runs all tests in the module " + name + ". Tests from upstream modules are _not_ run."
  )

  lazy val TestFailedSuitesOnly = Build(
    "Run failing test suites for " + name + " only", 
    () ⇒ Dependency.Graph.transitiveClosure(this, RunUnitTestsTask.failingTests(this)), 
    this,
    "testFailuresOnly",
    "Runs all failed tests in the module " + name
  )

  lazy val UpdateOnly =  Build(
    "Update libraries for " + name + " only",
    () ⇒ Dependency.Graph(UpdateTask(this)),
    this,
    "updateOnly",
    "Update libraries for " + name + " only"
  )

  def cleanOnly = CleanOnly.execute
  def testOnly = TestOnly.execute
  def testFailuredSuitesOnly = TestFailedSuitesOnly.execute
  def updateOnly = UpdateOnly.execute


  /********************
  *     Test classses 
  ********************/


  def testClassNames() = {
    testCompilePhase.classFiles.map(_.className(testOutputDir)).filterNot(_.contains("$")).filter(isAccessibleScalaTestSuite).toList
  }

  def constructorCodeAsString : String = throw new Exception("Only supported by test projects")

  /********************
  *     Paths and files
  ********************/

  def makerDirectory = mkdirs(rootAbsoluteFile, ".maker")
  def cacheDirectory = mkdirs(makerDirectory, "cache")

  def managedJars = findJars(managedLibDir)
  def classpathJars : Iterable[File] = findJars(unmanagedLibDirs.toSet + managedLibDir).toSet + props.ProjectScalaLibraryJar() + props.ProjectScalaCompilerJar() 

  def outputArtifact = file(packageDir.getAbsolutePath, name + ".jar")

  def publishLocalJarDir = file(publishLocalDir, "/jars/").makeDir
  def publishLocalJar = file(publishLocalJarDir, outputArtifact.getName)

  def sourceDir = file(rootAbsoluteFile, "src")
  def testSourceDir = file(rootAbsoluteFile, "tests")
  def targetDir = file(rootAbsoluteFile, "target-maker")
  def outputDir = file(targetDir, "classes")
  def testOutputDir = file(targetDir, "test-classes")
  def resourceDir = file(rootAbsoluteFile, "resources")
  def testResourceDir = file(rootAbsoluteFile, "test-resources")
  def docOutputDir = file(targetDir, "docs")
  def packageDir = file(targetDir, "package")
  def managedLibDir = file(rootAbsoluteFile, "lib_managed")
  def managedLibSourceDir = file(rootAbsoluteFile, "lib_src_managed")
  def managedResourceDir = file(rootAbsoluteFile, "resource_managed")
  def unmanagedLibDirs : Iterable[File] = List(file(rootAbsoluteFile, "lib"))
}


object Module{
 
  private val logger = ConsoleLogger()
  logger.setLevel(sbt.Level.Debug)
  val props = MakerProps()
  private val setup = Setup.create(
    props.ProjectScalaCompilerJar(),
    props.ProjectScalaLibraryJar(),
    Nil, 
    props.SbtInterfaceJar(),
    props.CompilerInterfaceSourcesJar(),
    props.JavaHome()
  )

  val compiler = Compiler.create(setup, logger)

  val analyses = new ConcurrentHashMap[File, Analysis]()

  def asClasspathStr(files : Iterable[File], sep : String = java.io.File.pathSeparator) =
    files.toList.distinct.toList.map(_.getAbsolutePath).sortWith(_.toString < _.toString).mkString(sep)

  def warnOfUnnecessaryDependencies(proj : Module){
    val log = proj.log

    proj.immediateUpstreamModules.foreach{
      p ⇒ 
        proj.immediateUpstreamModules.filterNot(_ == p).find(_.allUpstreamModules.contains(p)).foreach{
          p1 ⇒ 
            log.warn("Module " + proj.name + " doesn't need to depend on " + p.name + " as it is already inherited from " + p1.name)
        }
    }


    proj.immediateUpstreamTestModules.foreach{
      p ⇒ 
        proj.immediateUpstreamTestModules.filterNot(_ == p).find(_.allUpstreamTestModules.contains(p)).foreach{
          p1 ⇒ 
            log.warn("Module " + proj.name + " doesn't need a test dependency on " + p.name + " as it is already inherited from " + p1.name)
        }
    }

    val jarNames = proj.managedJars.map(_.getName).toSet
    proj.immediateUpstreamModules.foreach{
      upstreamModule ⇒
        val upstreamJarNames = upstreamModule.allUpstreamModules.flatMap(_.managedJars).map(_.getName).toSet
        val redundantJarNames = upstreamJarNames intersect jarNames
        if (redundantJarNames.nonEmpty)
          log.warn("Module " + proj.name + " doesn't need jars " + redundantJarNames.mkString(", ") + " as they are supplied by " + upstreamModule.name)
    }

  }

}

case class InvalidModuleException(msg : String) extends RuntimeException(msg)
