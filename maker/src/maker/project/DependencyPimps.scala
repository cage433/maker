package maker.project

import org.eclipse.aether.util.artifact.JavaScopes
import org.eclipse.aether.artifact.DefaultArtifact
import org.eclipse.aether.graph.{Exclusion, Dependency}
import scala.collection.JavaConversions._


case class RichDependency(
  org : String, 
  artifactId : String, 
  version : String, 
  useMajorScalaVersion : Boolean,
  scope : String = JavaScopes.COMPILE,
  classifier : String = "",
  extension : String = "",
  isOptional : Boolean = false,
  exclusions : Seq[Exclusion] = Nil
){
  def scalaVersionedArtifactId(majorScalaVersion : String) = 
    if (useMajorScalaVersion) s"${artifactId}_$majorScalaVersion" else artifactId

  def pomXml(majorScalaVersion : String) = 
    <dependency>
      <groupId>{org}</groupId>
      <artifactId>{scalaVersionedArtifactId(majorScalaVersion)}</artifactId>
      <version>{version}</version>
      <scope>compile</scope>
    </dependency>

  def dependency(majorScalaVersion : String) = {
    val artifact = new DefaultArtifact(
      org,
      scalaVersionedArtifactId(majorScalaVersion),
      classifier,
      extension,
      version
    )
    new Dependency(
      artifact,
      scope,
      isOptional,
      exclusions
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
    def withScope(scope : String) = new Dependency(
      dependency.getArtifact,
      scope,
      dependency.getOptional,
      dependency.getExclusions
    )
    def optional = new Dependency(
      dependency.getArtifact,
      dependency.getScope,
      true,
      dependency.getExclusions
    )
    def sourceDependency = {
      val artifact = dependency.getArtifact
      val sourceArtifact = new DefaultArtifact(
        artifact.getGroupId,
        artifact.getArtifactId,
        "sources",
        artifact.getExtension,
        artifact.getVersion
      )
      new Dependency(
        sourceArtifact,
        dependency.getScope,
        dependency.getOptional,
        dependency.getExclusions
      )
    }
    def withExclusions(groupAndArtifacts : String*) = {
      val exclusions = groupAndArtifacts.map{
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

      new Dependency(
        dependency.getArtifact,
        dependency.getScope,
        dependency.getOptional,
        exclusions
      )
    }

    def groupId = dependency.getArtifact.getGroupId
    def artifactId = dependency.getArtifact.getArtifactId
    def version = dependency.getArtifact.getVersion
    def extension = dependency.getArtifact.getExtension
    def classifier = Option(dependency.getArtifact.getClassifier).getOrElse("")
    def pomDependencyXML = {
      val artifact = dependency.getArtifact
      <dependency>
        <groupId>{artifact.getGroupId}</groupId>
        <artifactId>{artifact.getArtifactId}</artifactId>
        <version>{artifact.getVersion}</version>
        <scope>{dependency.getScope}</scope>
      </dependency>
    }
    def basename : String = s"$groupId-$artifactId-$version$classifier.$extension"
      //(groupId, artifactId, version, classifier.map("-" + _).getOrElse(""), extension)
  }
}

object DependencyPimps extends DependencyPimps
