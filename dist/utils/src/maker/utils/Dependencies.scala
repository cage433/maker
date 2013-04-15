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

package maker.utils

import org.apache.ivy.ant.IvyMakePom
import org.apache.ivy.plugins.parser.m2.PomWriterOptions
import xml.Elem
import java.io.File

trait MavenNamedElement { def mavenName : String }
trait MavenScope extends MavenNamedElement
case object CompileScope extends MavenScope { val mavenName = "compile" }
case object TestScope extends MavenScope { val mavenName = "test" }
case object ProvidedScope extends MavenScope { val mavenName = "provided" }
case object MavenSystemScope extends MavenScope { val mavenName = "system" }
case object UndefinedScope extends MavenScope { val mavenName = "" }

trait MavenType extends MavenNamedElement
case class MavenNamedType(mavenName : String) extends MavenType
case object MavenTestJarType extends MavenType { val mavenName = "test-jar" }

case class MavenNamedClassifier(mavenName : String) extends MavenNamedElement

case class ScalaVersion(version : Version)
object ScalaVersion {
  def apply(version : String) : ScalaVersion = ScalaVersion(Version(version))
}

// mvn group -> artifact -> version <-> ivy org -> name -> rev
object ModuleId {
  implicit def toGroupId(id : String) : GroupId = new GroupId(id)
  implicit def toGroupArtifactAndVersion(groupAndArtifact : GroupAndArtifact) : GroupArtifactAndVersion =
    GroupArtifactAndVersion(groupAndArtifact.groupId, groupAndArtifact.artifactId, None)
}
case class GroupId(id : String) {
  def %(artifactId : String) = GroupAndArtifact(this, ArtifactId(artifactId))
  def %%(artifactId : String)(implicit scalaVersion : ScalaVersion) = GroupAndArtifact(this, ArtifactId(artifactId + "_" + scalaVersion.version.version))
}
case class ArtifactId(id : String)
trait GAV {
  val groupId : GroupId
  val artifactId : ArtifactId
  val version : Option[Version] = None
  def toGroupAndArtifact = GroupAndArtifact(groupId, artifactId)
  def toGroupArtifactAndVersion = GroupArtifactAndVersion(groupId, artifactId, None)
  def withVersion(version : Version) = GroupArtifactAndVersion(groupId, artifactId, Some(version))

  def toIvyInclude : Elem = <dependency org={groupId.id} name={artifactId.id} rev={version.map(v => xml.Text(v.version))} />
  def toIvyExclude : Elem = <exclude org={groupId.id} module={artifactId.id} />
}
case class GroupAndArtifact(groupId : GroupId, artifactId : ArtifactId) extends GAV {
  def %(version : String) = GroupArtifactAndVersion(groupId, artifactId, Some(Version(version)))
  def %(maybeVersion : Option[String]) = GroupArtifactAndVersion(groupId, artifactId, maybeVersion.map(Version))
  override def toString = groupId.id + ":" + artifactId.id
}
case class Version(version : String)
case class GroupArtifactAndVersion(groupId : GroupId, artifactId : ArtifactId, override val version : Option[Version]) extends GAV {
  override def toString = groupId.id + ":" + artifactId.id + ":" + version.map(_.version).getOrElse("")
  def updateVersion(ver : Version) = this.copy(version = Some(ver))
  def toPath = groupId.id + File.separator + artifactId.id
  def groupAndArtifact = GroupAndArtifact(groupId, artifactId)
}

/**
 * Simple case class representing a library dependency definition (from a Maven repo)
 */
case class DependencyLib(
    name : String,
    gav : GroupArtifactAndVersion,
    scope : MavenScope,
    maybeOptional : Option[Boolean] = None,
    maybeType : Option[MavenType] = None,
    maybeClassifier : Option[MavenNamedClassifier] = None) {

  val classifier = maybeClassifier.getOrElse("")
  val version = gav.version.map(_.version).getOrElse("")

  def updateVersion(version : String) = this.copy(gav = this.gav.updateVersion(Version(version)))

  override def toString =
    "Name: %s, GAV: %s, Scope: %s, Optional: %s, %s %s"
      .format(
        name, gav.toString, scope,
        maybeOptional.map(o => "Optional: " + o),
        maybeType.map(t => "Type: " + t.mavenName),
        maybeClassifier.map(c => "Classifier: " + c.mavenName)
      )

  def toIvyMavenDependency : IvyMakePom#Dependency = {
    val ivyMakePom : IvyMakePom = new IvyMakePom
    val dep = new ivyMakePom.Dependency()
    dep.setGroup(gav.groupId.id)
    dep.setArtifact(gav.artifactId.id)
    dep.setVersion(version)
    dep.setScope(scope.mavenName)
    maybeOptional.map(b => dep.setOptional(b))
    maybeType.map(t => dep.setType(t.mavenName))
    maybeClassifier.map(c => dep.setClassifier(c.mavenName))
    dep
  }

  // note, ivy uses nulls to denote absence of a value, also type is not yet supported at all
  def toIvyPomWriterExtraDependencies : PomWriterOptions.ExtraDependency =
    new PomWriterOptions.ExtraDependency(
      gav.groupId.id, gav.artifactId.id, version,
      scope.mavenName,
      maybeType.map(_.mavenName).getOrElse(null),
      maybeClassifier.map(_.mavenName).getOrElse(null),
      maybeOptional.getOrElse(false))
}

object DependencyLib {
  implicit def toIvyMavenDependency(lib : DependencyLib) : IvyMakePom#Dependency =
    lib.toIvyMavenDependency
  implicit def toIvyPomWriterExtraDependencies(lib : DependencyLib) : PomWriterOptions.ExtraDependency =
    lib.toIvyPomWriterExtraDependencies
}
