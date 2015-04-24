package maker.project

import org.eclipse.aether.util.artifact.JavaScopes
import org.eclipse.aether.artifact.DefaultArtifact
import org.eclipse.aether.graph.{Exclusion, Dependency}
import scala.collection.JavaConversions._


trait DependencyPimps{
  class OrgAndArtifact(org : String, artifact : String){
    def %(version : String) = new Dependency(new DefaultArtifact(s"$org:$artifact:$version"), JavaScopes.COMPILE)
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
