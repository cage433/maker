package maker

import maker.utils.FileUtils._
import maker.utils.RichString._
import org.apache.commons.io.{FileUtils => ApacheFileUtils}
import java.io.File
import maker.utils.os.Command
import scala.xml.{Elem, NodeSeq}
import maker.project.Module
import maker.utils.{FileUtils, Int}
import scalaz.syntax.std.boolean._
import org.slf4j.{LoggerFactory, Logger}

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

    The jar will be cached at

      ~/.maker-resource-cache/org.scalacheck-scalacheck_2.9.2-1.9.jar
*/

// TODO - refactor all of this
case class Resource(
  groupId : String, 
  artifactId : String, 
  version : String, 
  downloadDirectory : Option[File] = None,
  extension : String = "jar",
  classifier : Option[String] = None,
  preferredRepository : Option[String] = None
) {
  import Resource._
  override def toString = s"Resource: $groupId $artifactId $version $extension $classifier $downloadDirectory $preferredRepository"
  lazy val log = LoggerFactory.getLogger(this.getClass)
  def relativeURL = "%s/%s/%s/%s-%s%s.%s" %
    (groupId.replace('.', '/'), artifactId, version, artifactId, version, classifier.map("-" + _).getOrElse(""), extension)

  def basename : String = "%s-%s-%s%s.%s" %
    (groupId, artifactId, version, classifier.map("-" + _).getOrElse(""), extension)

  def pomDependencyXML = {
    <dependency>
      <groupId>{groupId}</groupId>
      <artifactId>{artifactId}</artifactId>
      <version>{version}</version>
      <scope>compile</scope>
    </dependency>
  }

  def toIvyInclude : Elem =
    <dependency org={groupId} name={artifactId} rev={version}>
      {classifier.map(c => <artifact name={artifactId} type="jar" ext="jar" e:classifier={c} />).getOrElse(NodeSeq.Empty)}
    </dependency>

  def resolveVersions(versions : Map[String, String]) : Resource = {

    val Regex = "([^{]*)\\{([^}]*)\\}(.*)".r
    def resolve(s : String) : String = {
      s match {
        case Regex(before, versionId, after) => 
          val resolvedVersion = versions.getOrElse(versionId, throw new RuntimeException("Missing version for " + s))
          resolve(before + resolvedVersion + after)
        case _ => s
      }
    }

    copy(
      groupId=resolve(groupId), 
      artifactId=resolve(artifactId), 
      version=resolve(version)
    )
  }

  def isJarResource = extension == "jar"
  def isSourceJarResource = isJarResource && classifier == Some("sources")
  def isBinaryJarResource = isJarResource && ! isSourceJarResource

  lazy val resourceFile = downloadDirectory.map(FileUtils.file(_, basename)).getOrElse(???)

  /**
   * If the resource is not already in lib_managed (or equivalent) then try to copy from cache,
   * else download from external repository and put into cache.
   *
   * Note that source jars are only downloaded when we download a binary - this is as some
   * simply don't exist, and trying to download them every time we get an update can become
   * expensive if there are Nexus problems
   */
  def update(module : Module) : Resource.UpdateResult = {
    import module.props

    resourceFile.dirname.makeDirs

    val cachedFile = file(props.ResourceCacheDirectory(), resourceFile.basename)
    
    if (resourceFile.doesNotExist && cachedFile.exists){
      ApacheFileUtils.copyFileToDirectory(cachedFile, resourceFile.dirname)
    }

    if (resourceFile.exists){
      ResourceAlreadyExists
    } else {
      val errors = download(module)
      if (resourceFile.exists){
        log.info("Downloaded " + basename)
        if (cachedFile.doesNotExist)
          cacheDownloadedResource(props)
        ResourceDownloaded
      } else {
        ResourceFailedToDownload(errors)
      }
    }
  }

  private def cacheDownloadedResource(props : MakerProps){
    withTempDir{
      dir => 
        ApacheFileUtils.copyFileToDirectory(resourceFile, dir)
        // Hoping move is atomic
        ApacheFileUtils.moveFileToDirectory(file(dir, resourceFile.basename), props.ResourceCacheDirectory(), false)
    }
  }
  private def urls(module : Module) : List[String] = {
    val withExplicitVersion = resolveVersions(module.resourceVersions())
    (preferredRepository.map(module.resourceResolvers()(_)).toList ::: module.resourceResolvers().values.toList).map{
      repository => 
        repository + "/" + withExplicitVersion.relativeURL
    }
  }

  private def download(module : Module) = {
    var errors = List[(Int, String)]()  // (curl return code, cmd)
    urls(module).find{
      url =>
        val cmd = Command(
          "curl",
          "-s",
          "-L",
          "-H", "Pragma: no-cache",
          url,
          "-f",
          "-o",
          resourceFile.getAbsolutePath
        )
        val returnCode = cmd.exec 
        if (!resourceFile.exists)
          errors ::= (returnCode, url)
        resourceFile.exists
    }
    errors.reverse
  }
}

object Resource{
  sealed trait UpdateResult{
    def errors : List[(Int, String)] = Nil // (return code, curl command)
  }
  case object ResourceAlreadyExists extends UpdateResult
  case object ResourceDownloaded extends UpdateResult
  case class ResourceFailedToDownload(override val errors : List[(Int, String)]) extends UpdateResult

  def parse(s : String, resourceVersions : Map[String, String] = Map.empty, downloadDirectory : Option[File] = None) : Resource = {
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
      preferredRepository = optionalArgs.get("resolver"),
      downloadDirectory = downloadDirectory
    ).resolveVersions(resourceVersions)
  }
}
