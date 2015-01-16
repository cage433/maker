package maker.ivy

import maker.utils.FileUtils._
import scala.xml.{PrettyPrinter, NodeSeq}
import org.apache.commons.io.FileUtils._
import maker.project.Project
import maker.project.BaseProject
import java.io.File
import maker.project.Module


/**
 * utils for ivy, including ivy file generation for a project
 */
object IvyUtils {

  // TODO - check this is actually necessary, as the 
  // ivy file produced look somewhat odd
  def generateIvyFile(baseProject : BaseProject) : File = {
    val genFile = file(baseProject.rootAbsoluteFile, "ivy-dynamic.xml")

    val excludeDeps : List[NodeSeq] = baseProject.allUpstreamBaseProjects.flatMap(_.toIvyExclude).toList
    val ivyDepsXmlElems = baseProject match {
      case _ : Project => excludeDeps
      case m : Module => m.resources.map(_.toIvyInclude).toList ::: excludeDeps
    }

    val groupId = baseProject.groupId
    val artifactId = baseProject.artifactId


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
          <artifact name={artifactId} type="jar" ext="jar" e:classifier="source" conf="default" />
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
