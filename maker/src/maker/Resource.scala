package maker

import maker.utils.FileUtils._
import maker.utils.RichString._
import org.apache.commons.io.{FileUtils => ApacheFileUtils}
import java.io.File
import maker.utils.os.Command
import scala.xml.Elem
import scala.xml.NodeSeq
import maker.project.Module
import maker.utils.FileUtils
import maker.utils.FileUtils._

/*  
    Defines a resource held at some maven/nexus repository
      Maven      / Ivy
      groupId    / org      org.scalacheck
      artifactId / name     scalacheck_2.9.2
      version    / rev      1.9

    might be found at the URL

      http://repo1.maven.org/maven2/org/scalacheck/scalacheck_2.9.2/1.9/scalacheck_2.9.2-1.9.jar
      http://nexus.global.trafigura.com:8081/nexus/content/groups/mirror/starling/test/resources/bookmark-test-db/1.2/bookmark-test-db-1.2.gz


    It would be stored locally at 

    ./<project-dir>/lib_managed/org.scalacheck-scalacheck_2.9.2-1.9.jar
*/
case class Resource(
  groupId : String, 
  artifactId : String, 
  version : String, 
  extension : String = "jar",
  classifier : Option[String] = None,
  preferredRepository : Option[String] = None
) {
  def relativeURL = "%s/%s/%s/%s-%s.%s" % (groupId.replace('.', '/'), artifactId, version, artifactId, version, extension)
  def basename : String = "%s-%s-%s.%s" % (groupId, artifactId, version, extension)
  def pomDependencyXML : String = {
    """|<dependency>
       |  <groupId>%s</groupId>
       |  <artifactId>%s</artifactId>
       |  <version>%s</version>
       |  <scope>compile</scope>
       |</dependency>""".stripMargin % (groupId, artifactId, version)
  }

  def toIvyInclude : Elem =
    <dependency org={groupId} name={artifactId} rev={version.map(v => xml.Text(version))}>
      {classifier.map(c => <artifact name={artifactId} type="jar" ext="jar" e:classifier={c} />).getOrElse(NodeSeq.Empty)}
    </dependency>

  def resolveVersions(versions : Map[String, String]) : Resource = {

    val Regex = "([^{]*)\\{([^}]*)\\}(.*)".r
    def resolve(s : String) : String = {
      s match {
        case Regex(before, versionId, after) => 
          val resolvedVersion = versions.getOrElse(versionId, throw new RuntimeException("Missing version for " + versionId))
          resolve(before + resolvedVersion + after)
        case _ => s
      }
    }
    copy(groupId=resolve(groupId), artifactId=resolve(artifactId), version=resolve(version))
  }

  def resourceFile(module : Module) = {
    (extension, classifier) match {
      case (_, Some("sources")) => 
        FileUtils.file(module.managedLibSourceDir, basename)
      case ("jar", _) => 
        FileUtils.file(module.managedLibDir, basename)
      case _ => 
        FileUtils.file(module.managedResourceDir, basename)
    }
  }
  def update(module : Module){

    val props = module.props

    val resourceFile_ = resourceFile(module)
    resourceFile_.dirname.makeDirs

    val cachedFile = file(props.ResourceCacheDirectory(), resourceFile_.basename)

    def download() {
      (preferredRepository.toList ::: props.resourceResolvers().values.toList).find{
        repository => 
          val cmd = Command(
            props, 
            "curl",
            "-s",
            "-H", "Pragma: no-cache",
            repository + "/" + relativeURL,
            "-f",
            "-o",
            resourceFile_.getAbsolutePath
          )
          cmd.exec 
          resourceFile_.exists
      }
    }

    if (resourceFile_.doesNotExist && cachedFile.exists)
      ApacheFileUtils.copyFileToDirectory(cachedFile, resourceFile_.dirname)

    if (resourceFile_.doesNotExist)
      download()

    if (resourceFile_.exists)
      ApacheFileUtils.copyFileToDirectory(resourceFile_, props.ResourceCacheDirectory())

  }

  //def file(module : Module) : File = {
    //(extension, classifier) match {
      //case (_, Some("sources")) => 
      //FileUtils.file(module.managedLibSourceDir, basename)
      //case ("jar", _) => 
      //FileUtils.file(module.managedLibDir, basename)
      //case _ => 
      //FileUtils.file(module.managedResourceDir, basename)
      //}
      //}

}

object Resource{
  def build(s : String, resourceVersions : Map[String, String] = Map.empty, resourceResolvers : Map[String, String] = Map.empty) : Resource = {
    def exitWithBadUsage{
      val errorMessage = """|Valid resource format is 
                            | <group-id> <artifact-id> <version> [resolver:<resolver-name>] [type:<type-name - e.g. jar, xml, gz>]
                            |
                            | Was given [""".stripMargin + s + "]"
      throw new RuntimeException(errorMessage)
    }

    val terms : List[String] = s.split(" ").toList.filterNot(_ == "")

      if (terms.size < 3 
        || terms.take(3).exists(_.contains(':')) 
        || !terms.drop(3).forall(_.contains(':')))
    {
      exitWithBadUsage
    }

    val List(groupId, artifactId, version) = terms.take(3)

    val optionalArgs = terms.drop(3).map{term => 
      val List(key, value) = term.split(':').toList
        key -> value
    }.toMap

    Resource(
      groupId, artifactId, version,
      extension = optionalArgs.getOrElse("type", "jar"),
      classifier = optionalArgs.get("classifier"),
      preferredRepository = optionalArgs.get("resolver").map(resourceResolvers(_))
    ).resolveVersions(resourceVersions)
  }
}
