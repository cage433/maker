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
import maker.utils.FileUtils._
import maker.MakerProps
import sbt.ConsoleLogger
import scala.collection.JavaConversions._
import com.typesafe.zinc.Compiler
import maker.task.Dependency
import java.util.concurrent.ConcurrentHashMap
import sbt.inc.Analysis
import maker.task.compile._
import com.typesafe.zinc.Setup
import maker.task.Build
import maker.task.tasks.CleanTask
import maker.task.tasks.RunUnitTestsTask
import maker.task.tasks.UpdateTask
import maker.Resource
import maker.PomUtils
import maker.task.BuildResult
import maker.task.Task

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

  protected val upstreamModulesForBuild = List(this)

  val resourcesFile = file(root, "external-resources")

  def resources() : List[Resource]  = {
    
    val resources = resourcesFile.readLines.toList.filterNot{
      line => 
        line.startsWith("#") || line.trim.size == 0
    }.map(Resource.build(this, _, props.resourceVersions(), props.resourceResolvers()))
    resources.distinct
  }



  Module.warnOfUnnecessaryDependencies(this)

  def javacOptions : List[String] = Nil 
  def scalacOptions : List[String] = Nil
  /**
   * The standard equals method was slow, making Dependency operations very expensive.
   */
   override def equals(rhs : Any) = {
     rhs match {
       case p : Module if p.root == root => {
         //I believe this assertion should always hold. It's really here so that
         //this overriden equals method never returns true on differing modules.
         assert(this eq p, "Shouldn't have two modules pointing to the same root")
         true
       }
       case _ => false
     }
   }

  override def hashCode = root.hashCode

  private def warnOfRedundantDependencies() {
    immediateUpstreamModules.foreach{
      module =>
        val otherUpstreamModules = immediateUpstreamModules.filterNot(_ == module)
        otherUpstreamModules.find(_.allUpstreamModules.contains(module)) match {
          case Some(otherUpstreamModule) =>
          log.warn(name + " shouldn't depend on " + module.name + " as it is inherited via " + otherUpstreamModule.name)
          case None =>
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

  /**
    * Execute just this single task, none of its upstream 
    * dependencies. 
    * In general should only be used by devs at the REPL - should
    * not be used as the basis for more complex builds
    */
  private def executeSansDependencies(task : Task) : BuildResult = {
    val build = Build(
      task.name + " for " + name + " only",
      Dependency.Graph(task),
      props.NumberOfTaskThreads()
    )
    execute(build)
  }

  def cleanOnly = executeSansDependencies(CleanTask(this))
  def testOnly(verbose : Boolean) : BuildResult = executeWithDependencies(RunUnitTestsTask(this, verbose))
  def testOnly :BuildResult = testOnly(false)
  def testFailuredSuitesOnly(verbose : Boolean) : BuildResult = executeSansDependencies(
    RunUnitTestsTask.failingTests(this, verbose)
  )
  def testFailuredSuitesOnly : BuildResult = testFailuredSuitesOnly(false)
  def updateOnly = executeSansDependencies(UpdateTask(this, forceSourceUpdate = false))


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
  def classpathJars : Iterable[File] = findJars(unmanagedLibDirs.toSet + managedLibDir).toSet + props.ProjectScalaLibraryJar() + props.ProjectScalaCompilerJar() + props.ProjectScalaReflectJar()


  // for the managed (non-jar) resources
  def includeInMainJar(file: File): Boolean = true
  def outputArtifact = file(packageDir.getAbsolutePath, name + ".jar")
  def includeInTestJar(file: File): Boolean = false
  def testOutputArtifact = file(packageDir.getAbsolutePath, name + "-test.jar")

  def publishLocalJarDir = file(publishLocalDir, "/jars/").makeDir
  def publishLocalJar = file(publishLocalJarDir, outputArtifact.getName)

  def sourceDirs: List[File] =
    List(file(rootAbsoluteFile, "src/main/scala"), file(rootAbsoluteFile, "src/main/java"))
  def testSourceDirs: List[File] =
    List(file(rootAbsoluteFile, "src/test/scala"), file(rootAbsoluteFile, "src/test/java"))
  def resourceDir = file(rootAbsoluteFile, "src/main/resources")
  def testResourceDir = file(rootAbsoluteFile, "src/test/resources")

  def targetDir = file(rootAbsoluteFile, "target-maker")
  def outputDir = file(targetDir, "classes")
  def testOutputDir = file(targetDir, "test-classes")
  def docOutputDir = file(targetDir, "docs")
  def packageDir = file(targetDir, "package")
  def managedLibDir = file(rootAbsoluteFile, "lib_managed")
  def managedLibSourceDir = file(rootAbsoluteFile, "lib_src_managed")
  def managedResourceDir = file(rootAbsoluteFile, "resource_managed")
  def unmanagedLibDirs : Iterable[File] = List(file(rootAbsoluteFile, "lib"))
  def warnUnnecessaryResources = true
}

trait ClassicLayout {
  this: Module =>
  override def sourceDirs: List[File] = file(rootAbsoluteFile, "src") :: Nil
  override def testSourceDirs: List[File] = file(rootAbsoluteFile, "tests") :: Nil
  override def resourceDir = file(rootAbsoluteFile, "resources")
  override def testResourceDir = file(rootAbsoluteFile, "test-resources")
}


object Module{
 
  private val logger = ConsoleLogger()
  logger.setLevel(sbt.Level.Debug)
  val props = MakerProps()

  // commented code is for 2.10 with zinc 0.3.x
  private val setup = Setup.create/*setup*/(
    props.ProjectScalaCompilerJar(),
    props.ProjectScalaLibraryJar(),
    List(props.ProjectScalaReflectJar()), 
    props.SbtInterfaceJar(),
    props.CompilerInterfaceSourcesJar(),
    props.JavaHome(), 
    forkJava = false
  )

  val compiler = Compiler.create(setup, logger)

  val analyses = new ConcurrentHashMap[File, Analysis]()

  def asClasspathStr(files : Iterable[File], sep : String = java.io.File.pathSeparator) =
    files.toList.distinct.toList.map(_.getAbsolutePath).sortWith(_ < _).mkString(sep)

  def warnOfUnnecessaryDependencies(proj : Module){
    val log = proj.log

    proj.immediateUpstreamModules.foreach{
      p => 
        proj.immediateUpstreamModules.filterNot(_ == p).find(_.allUpstreamModules.contains(p)).foreach{
          p1 => 
            log.warn("Module " + proj.name + " doesn't need to depend on " + p.name + " as it is already inherited from " + p1.name)
        }
    }


    proj.immediateUpstreamTestModules.foreach{
      p => 
        proj.immediateUpstreamTestModules.filterNot(_ == p).find(_.allUpstreamTestModules.contains(p)).foreach{
          p1 => 
            log.warn("Module " + proj.name + " doesn't need a test dependency on " + p.name + " as it is already inherited from " + p1.name)
        }
    }

    val jarNames = proj.managedJars.map(_.getName).toSet
    proj.immediateUpstreamModules.foreach{
      upstreamModule =>
        val upstreamJarNames = upstreamModule.allUpstreamModules.flatMap(_.managedJars).map(_.getName).toSet
        val redundantJarNames = upstreamJarNames intersect jarNames
        if (redundantJarNames.nonEmpty && proj.warnUnnecessaryResources)
          log.warn("Module " + proj.name + " doesn't need jars " + redundantJarNames.mkString(", ") + " as they are supplied by " + upstreamModule.name)
    }

  }

}

case class InvalidModuleException(msg : String) extends RuntimeException(msg)
