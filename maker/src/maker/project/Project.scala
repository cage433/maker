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
import maker.utils.ModuleId._
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
import maker.utils.ivy.IvyReader
import maker.utils.maven._
import scalaz.Scalaz._
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

/**
  * Defines a software project.
  *
  * A project can either be a self contained piece of software, or a module
  * in a larger project. Each project knows its upstream dependencies, these
  * are projects which must be compiled before it - and whose classes are on this
  * project's classpath.
  * 
  * Most of the implementation is contained in the traits that Project implements. This
  * has been done purely for code layout reasons - Project.scala became extremely large
  * and so it its functionality was split into areas of common interest.
  *
  * @constructor create a new project instance
  * @param root the root directory of the project
  * @param name the project's name - used in logging
  * @param layout the structure of the project's source, class and resource files
  * @param props    a set of properties governing how maker behaves and describing its environment. Multi module projects will normally have most of these properties identical
  * @param moduleIdentity   identity in the sense of Maven or Ivy. Published POM includes this in its name - if none is supplied then a reasonable default is invented from Props
  * @param dependencyAdjustments  a place to put adjustments to an ivy file - required if that file is not under our
  *        our control. e.g. if generated from a POM. [[maker.project.DependencyAdjustments]]
  */

class Project(
    private val root : File,
    val name : String,
    val layout : ProjectLayout,
    val upstreamProjects : List[Project] = Nil,
    val upstreamTestProjects : List[Project] = Nil,
    val props : MakerProps = MakerProps(),
    val moduleIdentity : Option[GroupAndArtifact] = None, 
    val dependencyAdjustments : DependencyAdjustments = DependencyAdjustments.Null,
    val analyses : ConcurrentHashMap[File, Analysis] = Project.analyses
) 
  extends ProjectSugar
  with ProjectTasks
  with ProjectIvy
  with ProjectTestHelper
  with ProjectFilesAndPaths
  with ProjectMetaData
  with ProjectTaskDependencies
{
  val rootAbsoluteFile = root.asAbsoluteFile

  val log = props.log

  /**
   * The standard equals method was slow, making Dependency operations very expensive.
   */
   override def equals(rhs : Any) = {
     rhs match {
       case p : Project if p.root == root ⇒ {
         //I believe this assertion should always hold. It's really here so that
         //this overriden equals method never returns true on differing projects.
         assert(this eq p, "Shouldn't have two projects pointing to the same root")
         true
       }
       case _ ⇒ false
     }
   }

  override def hashCode = root.hashCode

  private def warnOfRedundantDependencies() {
    upstreamProjects.foreach{
      immediateUpstreamProj ⇒
        val otherUpstreamProjects = upstreamProjects.filterNot(_ == immediateUpstreamProj)
        otherUpstreamProjects.find(_.allUpstreamProjects.contains(immediateUpstreamProj)) match {
          case Some(otherUpstreamProj) ⇒
          log.warn(name + " shouldn't depend on " + immediateUpstreamProj.name + " as it is inherited via " + otherUpstreamProj.name)
          case None ⇒
        }
    }
  }

  warnOfRedundantDependencies()

  def testCompilePhase = ProjectPhase(this, TestCompilePhase)
  def compilePhase = ProjectPhase(this, SourceCompilePhase)

  def allStrictlyUpstreamProjects : List[Project] = upstreamProjects.flatMap(_.allUpstreamProjects).distinct.sortWith(_.name < _.name)
  def allUpstreamProjects         : List[Project] = this :: allStrictlyUpstreamProjects

  def testOnlyUpStreamProjects : List[Project] = {
    val allUpstream = allUpstreamProjects

    upstreamTestProjects.filterNot(tp => allUpstream.contains(tp))
  }

  override def toString = name
  def downstramSourceDeps(sourceFile : File){
    val analysis = Compiler.analysis(compilePhase.compilationCacheFile)
  }
}


object Project{
 
  private val logger = ConsoleLogger()
  logger.setLevel(sbt.Level.Debug)
  val props = MakerProps()
  private val setup = Setup.create(
    props.ScalaCompilerJar(),
    props.ScalaLibraryJar(),
    Nil, 
    props.SbtInterfaceJar(),
    props.CompilerInterfaceSourcesJar(),
    props.JavaHome()
  )

  val compiler = Compiler.create(setup, logger)

  val analyses = new ConcurrentHashMap[File, Analysis]()

  def asClasspathStr(files : Iterable[File], sep : String = ":") =
    files.toList.map(_.getAbsolutePath).sortWith(_.toString < _.toString).mkString(sep)

}


/**
 * Allows adjustments to the Ivy file - needed where it is not under our control
 * e.g. if it is generated from a POM
 */
case class DependencyAdjustments(
  additionalLibs : List[GAV] = Nil,
  additionalExcludedLibs : List[GAV] = Nil,
  providedLibNames : List[String] = Nil // don't package any of these named jars (in wars, useful when running webapps using a single classpath)
) {
  def additionalLibs(libs : GAV*) = copy(additionalLibs = additionalLibs ++ libs)
  def withAdditionalExcludedLibs(libs : GAV*) = copy(additionalExcludedLibs = additionalExcludedLibs ++ libs)
  def withProvidedLibNames(libNames : String*) = copy(providedLibNames = providedLibNames ++ libNames)
}
object DependencyAdjustments{
  val Null = DependencyAdjustments()
}

case class WebAppDetails(directory : File, port : Int)
