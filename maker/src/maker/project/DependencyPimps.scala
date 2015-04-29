package maker.project

import org.eclipse.aether.util.artifact.JavaScopes
import org.eclipse.aether.artifact.DefaultArtifact
import org.eclipse.aether.graph.{Exclusion, Dependency}
import scala.collection.JavaConversions._
import maker.ScalaVersion


case class RichDependency(
  org : String, 
  artifactId : String, 
  version : String, 
  useMajorScalaVersion : Boolean,
  scope : String = JavaScopes.COMPILE,
  classifier : String = "",
  extension : String = "jar",
  isOptional : Boolean = false,
  toExclude : Seq[String] = Nil // Of the form "group:artifact" or "group"
){

  // Note the long string has no mention of classifier or optional
  def toLongString = {
    var s = s""""$org" % "$artifactId" ${if (useMajorScalaVersion) "%%" else "%"} "$version""""
    if (scope != JavaScopes.COMPILE) 
      s += s""" withScope("${scope}")"""
    if (toExclude.nonEmpty)
      s += toExclude.mkString(" withExclusions(", ",", ")")
    s
  }

  def withScope(scope : String) = copy(scope = scope)
  def scalaVersionedArtifactId(scalaVersion : ScalaVersion) = 
    if (useMajorScalaVersion) s"${artifactId}_${scalaVersion.versionBase}" else artifactId

  def excluding(groupAndArtifacts : String*) = copy(toExclude = groupAndArtifacts)

  def makeExclusions : Seq[Exclusion] = {
    toExclude.map{
      gAndA => 
        gAndA.split(":") match {
          case Array(group, artifact) => 
            new Exclusion(group, artifact, "*", "*")
          case Array(group) => 
            new Exclusion(group, "*", "*", "*")
          case other =>
            ???
        }
    }
  }

  def pomXml(scalaVersion : ScalaVersion) = 
    <dependency>
      <groupId>{org}</groupId>
      <artifactId>{scalaVersionedArtifactId(scalaVersion)}</artifactId>
      <version>{version}</version>
      <scope>compile</scope>
    </dependency>

  def dependency(scalaVersion : ScalaVersion) = {
    val artifact = new DefaultArtifact(
      org,
      scalaVersionedArtifactId(scalaVersion),
      classifier,
      extension,
      version
    )
    new Dependency(
      artifact,
      scope,
      isOptional,
      makeExclusions
    )
  }
}
trait DependencyPimps{
  class OrgAndArtifact(org : String, artifact : String){
    def %(version : String) = new RichDependency(org, artifact, version, useMajorScalaVersion = false)
    def %%(version : String) = new RichDependency(org, artifact, version, useMajorScalaVersion = true)
  }
  implicit class Organization(name : String){
    def %(artifact : String) = new OrgAndArtifact(name, artifact)
  }
  implicit class PimpedDependency(dependency : Dependency){
    def groupId = dependency.getArtifact.getGroupId
    def artifactId = dependency.getArtifact.getArtifactId
    def version = dependency.getArtifact.getVersion
    def toLongString = s"$groupId, $artifactId, $version, scope = ${dependency.getScope}"
  }
}

object DependencyPimps extends DependencyPimps
