package maker.ivy

import java.io.File
import maker.utils.FileUtils._
import scala.xml.{PrettyPrinter, NodeSeq}
import org.apache.commons.io.FileUtils._
import maker.project.Project


/**
 * utils for ivy, including ivy file generation for a project
 */
object IvyUtils {

  val IVY_FILE_EXT = "-dynamic.xml"
  def generateIvyFile(project : Project) : File = {
    val layout = project.layout
    val genFile = file(layout.targetDir, nameAndExt(layout.ivyDepsFile)._1 + IVY_FILE_EXT)

    val ivyDeps = project.readIvyDependencies()

    val includeDeps : List[NodeSeq] = (project.dependencies.libs.map(_.toIvyInclude) ::: ivyDeps.map(_.gav.toIvyInclude)).distinct
    val excludeDeps : List[NodeSeq] = (project.allUpstreamProjects.flatMap(_.moduleId.toIvyExclude) :::
      project.dependencies.excludedLibs.map(_.toIvyExclude)).distinct

    val id = project.moduleIdentity
    val groupId = id.map(_.groupId.id).getOrElse("undefined")
    val artifactId = id.map(_.artifactId.id).getOrElse("undefined")

    val ivyDepsXmlElems = (includeDeps ::: excludeDeps)

    // Todo: some of this is currently 'hard-coded' to our build...
    val ivyFileXML =
      <ivy-module version="1.0" xmlns:e="http://ant.apache.org/ivy/extra">
        <info organisation={groupId} module={artifactId} revision="${maker.module.version}" />

        <configurations>
          <conf name="default" transitive="false"/>
          <conf name="compile" transitive="false"/>
          <conf name="test" transitive="false"/>
        </configurations>

        <publications>
          <artifact name={artifactId} type="pom"/>
          <artifact name={artifactId} type="jar" ext="jar" conf="default" />
        </publications>

        <dependencies  defaultconfmapping="${ivy.default.conf.mappings}" >
          {ivyDepsXmlElems}
        </dependencies>
      </ivy-module>

    val xmlPrinter = new PrettyPrinter(160, 2).format(ivyFileXML)
    write(genFile, xmlPrinter.format(ivyFileXML))
    genFile
  }
}
