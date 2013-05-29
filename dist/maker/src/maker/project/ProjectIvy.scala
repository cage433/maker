package maker.project

import maker.utils.GroupAndArtifact
import java.io.File
import maker.utils.DependencyLib
import maker.utils.ivy.IvyReader
import maker.utils.maven.MavenRepository
import maker.utils.maven.ModuleDef
import maker.utils.maven.ProjectDef
import maker.utils.maven.ScmDef
import maker.utils.ModuleId._
import maker.utils.ivy.IvyReader._

/// some ivy/maven related utils
trait ProjectIvy {
  self : Project ⇒

    def description : String = name

    private val ivySourceFile = layout.ivyDepsFile

    lazy val moduleId : GroupAndArtifact = moduleIdentity.getOrElse(props.GroupId() % name)

    def readIvyDependencies(subsProps : Boolean = true) : List[DependencyLib] = {
      import IvyReader._
      if (ivySourceFile.exists()) {
        val ivyProps = if (subsProps) readIvyProperties() else Map[String, String]()
        readIvyDependenciesFromFile(log, ivySourceFile, ivyProps)
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
      extractIvyPropertiesFromFile(log, ivySourceFile)

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
