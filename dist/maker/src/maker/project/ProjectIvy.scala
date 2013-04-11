package maker.project

import maker.utils.GroupAndArtifact
import java.io.File
import maker.utils.FileUtils._
import org.apache.commons.io.FileUtils._
import scala.xml.NodeSeq
import maker.MakerProps
import maker.utils.DependencyLib
import maker.utils.ivy.IvyReader
import maker.utils.maven.MavenRepository
import maker.utils.maven.ModuleDef
import maker.utils.maven.ProjectDef
import maker.utils.maven.ScmDef
import maker.utils.GroupId
import maker.utils.ModuleId._
import maker.utils.Utils
import maker.utils.ivy.IvyReader._
import maker.task.compile._

/// some ivy/maven related utils
trait ProjectIvy{
  self : Project ⇒

    val generatedIvyExtension = "-dynamic.ivy"

    def description : String = name
    lazy val moduleId : GroupAndArtifact = moduleIdentity.getOrElse(props.GroupId() % name)

    def ivyGeneratedFile : Option[File] = {
      def createNewDynamicFile(dynamicIvyFile : File){
        copyFile(layout.ivyFile, dynamicIvyFile)
        val includes : List[NodeSeq] = allUpstreamProjects.flatMap(_.dependencyAdjustments.additionalLibs).map(_.toIvyInclude)
        val excludes : List[NodeSeq] =
          allUpstreamProjects.map(_.moduleId.toIvyExclude) ::: allUpstreamProjects.flatMap(_.dependencyAdjustments.additionalExcludedLibs.map(_.toIvyExclude))

        // todo: make this happen without requiring placeholder text
        replaceInFile(dynamicIvyFile, "${maker.module.excluded.libs}", (includes ::: excludes).mkString("\n"))
      }

      synchronized {
        if (layout.ivyFile.exists) {
          val dynamicIvyFile = file(layout.targetDir, nameAndExt(layout.ivyFile)._1 + generatedIvyExtension)
          if (!dynamicIvyFile.exists || dynamicIvyFile.lastModified < layout.ivyFile.lastModified){
            createNewDynamicFile(dynamicIvyFile)
          }
          Some(dynamicIvyFile)
        } else {
          None
        }
      }
    }

    def readIvyDependencies(subsProps : Boolean = true) : List[DependencyLib] = {
      import IvyReader._
      val ivyFile = ivyGeneratedFile.getOrElse(layout.ivyFile)
      if (ivyFile.exists()) {
        val ivyProps = if (subsProps) readIvyProperties() else Map[String, String]()
        readIvyDependenciesFromFile(log, ivyFile, ivyProps)
      }
      else {
        log.info("No Ivy config for this module")
        Nil
      }
    }

    def readIvyResolvers() : List[MavenRepository] = {
      if (layout.ivySettingsFile.exists())
        IvyReader.readIvyResolversFromFile(log, layout.ivySettingsFile)
      else {
        log.info("No Ivy config")
        Nil
      }
    }

    def readIvyProperties(extractModuleProps : Boolean = false) : Map[String, String] = {
      if (extractModuleProps)
        extractInModuleProperties()
      else
        readIvySettingsProperties()
    }
    def readIvySettingsProperties() : Map[String, String] =
      readIvyPropertiesFromFile(log, layout.ivySettingsFile)
    def extractInModuleProperties() : Map[String, String] =
      extractIvyPropertiesFromFile(log, layout.ivyFile)

    /**
     * maven / ivy integration
     *   get a module definition structure containing details of this module, direct dependencies and
     *   upstream module dependencies as required to produce published module artifacts, maven poms etc
     */
    def moduleDef(ver : Option[String] = None, includeUpstreamModuleTransitives : Boolean = false, extractProps : Boolean = false) : ModuleDef = {
      import maker.utils._
      val version = ver.getOrElse(props.Version())
      val deps : List[DependencyLib] = readIvyDependencies()
      val repos : List[MavenRepository] = readIvyResolvers()
      val moduleLibDef = DependencyLib(name, moduleId % version, CompileScope)

      val moduleDeps = {
        // watch out for unmanaged local libs, we have no easy (or recommended) way to translate them into maven/ivy worlds
        val unmanagedLibs : List[File] = layout.unmanagedLibDirs.filter(_.exists).toList.flatMap(_.listFiles.toList)
        val unmanagedLocalLibs = unmanagedLibs.map(f => DependencyLib(name, this.moduleId.groupId % f.getName, MavenSystemScope))
        if (!unmanagedLocalLibs.isEmpty) {
          log.warn("The following are unmanaged local libs, these will not get translated into maven based artifacts")
          log.info(unmanagedLocalLibs.mkString("\n"))
        }

        // a bit hardcoded still, but to get a basic maven build working, test module dependencies need to result in additional "test" scope, "test-jar" type maven dependencies
        val immediateUpstreamCompileModules : List[DependencyLib] = upstreamProjects.map(c => DependencyLib(name, c.moduleId % version, CompileScope))
        val immediateUpstreamTestModules : List[DependencyLib] = upstreamTestProjects.map(c => DependencyLib(name, c.moduleId % version, TestScope, maybeType = Some(MavenTestJarType)))
        val immediateUpstreamModules = immediateUpstreamCompileModules ::: immediateUpstreamTestModules

        val allUpstreamLibs : List[DependencyLib] = if (includeUpstreamModuleTransitives) allStrictlyUpstreamProjects.flatMap(_.readIvyDependencies(!extractProps)) else Nil
        val allProjectModules : List[DependencyLib] = (immediateUpstreamModules ::: allUpstreamLibs).distinct

        if (compilePhase.scalaFiles.size > 0)
          DependencyLib(name, "org.scala-lang" % "scala-library" % props.ScalaVersion(), CompileScope) :: allProjectModules
        else
          allProjectModules
      }
      val projectDef = ProjectDef(description, moduleLibDef, moduleDeps)
      ModuleDef(projectDef, deps, repos, ScmDef(props.ScmUrl(), props.ScmConnection()), props.Licenses(), props.Developers())
    }
}
