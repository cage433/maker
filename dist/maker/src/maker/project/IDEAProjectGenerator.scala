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

package maker.project

import maker.utils.FileUtils._
import java.io.File
import maker.utils.FileUtils
import collection.mutable.ListBuffer
import maker.task.compile._
import maker.utils.RichString._
import maker.MakerProps

case class IDEAProjectGenerator(props : MakerProps) {
  val scalaVersion = props.ScalaVersion()

  def generateTopLevelModule(rootDir:File, name:String, excludedFolders:List[String]) {

    val formattedExcludedFolders = if (excludedFolders.nonEmpty) {
      val excludedFoldersString = excludedFolders.map(folder => {
        """      <excludeFolder url="file://$MODULE_DIR$/%s" />""" % folder
      }).mkString("\n")
      """    <content url="file://$MODULE_DIR$">
%s
    </content>""" % excludedFoldersString
    } else {
        """<content url="file://$MODULE_DIR$" />"""
    }

    val content = """<?xml version="1.0" encoding="UTF-8"?>
<module type="JAVA_MODULE" version="4">
  <component name="NewModuleRootManager" inherit-compiler-output="true">
    <exclude-output />
%s
    <orderEntry type="inheritedJdk" />
    <orderEntry type="sourceFolder" forTests="false" />
  </component>
</module>
""".format(formattedExcludedFolders)

    writeToFileIfDifferent(file(rootDir, name + ".iml"), content)
  }

  def generateIDEAProjectDir(rootDir:File, name:String) {
    val ideaProjectRoot = mkdirs(file(rootDir, ".idea"))
    writeToFileIfDifferent(file(ideaProjectRoot, ".name"), name)

    // For now we are insisting that there is a "scala directory" at this location: $PROJECT_DIR$/lib/scala/lib_managed/
    // Obviously, this isn't very good and we should allow it to be specified.
    List(props.ScalaLibraryJar(), props.ScalaCompilerJar()).foreach{
      jar ⇒ 
      if (!jar.exists) {
        throw new Exception("Maker requires that file " + jar + " + be present. Launch maker with the '-y' flag to download it")
      }
    }

    val librariesDir = mkdirs(file(ideaProjectRoot, "libraries"))
    val scalaLibraryContent = """<component name="libraryTable">
  <library name="scala-library-%s">
    <CLASSES>
      <root url="jar://$PROJECT_DIR$/scala-lib/scala-library-%s.jar!/" />
    </CLASSES>
    <JAVADOC />
    <SOURCES>
      <root url="jar://$PROJECT_DIR$/scala-lib/scala-library-%s-sources.jar!/" />
    </SOURCES>
  </library>
</component>
""" % (scalaVersion, scalaVersion, scalaVersion)
    writeToFileIfDifferent(file(librariesDir, "scala_library_%s.xml" % scalaVersion.replace('.', '_')), scalaLibraryContent)

    val scalaCompilerLibraryContent = """<component name="libraryTable">
  <library name="scala-compiler-%s">
    <CLASSES>
      <root url="jar://$PROJECT_DIR$/zinc-libs/scala-compiler-%s.jar!/" />
      <root url="jar://$PROJECT_DIR$/scala-lib/scala-library-%s.jar!/" />
    </CLASSES>
    <JAVADOC />
    <SOURCES />
  </library>
</component>
""" % (scalaVersion, scalaVersion, scalaVersion)
    writeToFileIfDifferent(file(librariesDir, "scala_compiler_%s.xml" % scalaVersion.replace('.', '_')), scalaCompilerLibraryContent)

    val scalaCompilerContent = """<?xml version="1.0" encoding="UTF-8"?>
<project version="4">
  <component name="ScalacSettings">
    <option name="COMPILER_LIBRARY_NAME" value="scala-compiler-%s" />
    <option name="COMPILER_LIBRARY_LEVEL" value="Project" />
    <option name="MAXIMUM_HEAP_SIZE" value="2048" />
    <option name="FSC_OPTIONS" value="-max-idle 0" />
  </component>
</project>
""" % scalaVersion
    writeToFileIfDifferent(file(ideaProjectRoot, "scala_compiler.xml"), scalaCompilerContent)

    val miscContent = """<?xml version="1.0" encoding="UTF-8"?>
<project version="4">
  <component name="ProjectRootManager" version="2" languageLevel="JDK_1_6" assert-keyword="true" jdk-15="true" project-jdk-name="JDK6" project-jdk-type="JavaSDK">
    <output url="file://$PROJECT_DIR$/out" />
  </component>
</project>
"""
    writeMiscFileIfRequired(file(ideaProjectRoot, "misc.xml"), miscContent)

    var gitDirFound = false
    var gitDir = rootDir.getAbsoluteFile
    while (!gitDirFound && (gitDir != null)) {
      val fileToCheck = file(gitDir, ".git")
      if (fileToCheck.exists) {
        gitDirFound = true
      } else {
        gitDir = gitDir.getParentFile
      }
    }
    if (gitDir != null) {
      val relDirectory = {
        val rd = gitDir.relativeTo(rootDir).toString
        if (rd.isEmpty) {
          ""
        } else {
          "$PROJECT_DIR$/" + rd
        }
      }

      val vcsContent = """<?xml version="1.0" encoding="UTF-8"?>
<project version="4">
  <component name="VcsDirectoryMappings">
    <mapping directory="%s" vcs="Git" />
  </component>
</project>
""".format(relDirectory)
      writeToFileIfDifferent(file(ideaProjectRoot, "vcs.xml"), vcsContent)
    }

    val highlightingContent = """<?xml version="1.0" encoding="UTF-8"?>
<project version="4">
  <component name="HighlightingAdvisor">
    <option name="SUGGEST_TYPE_AWARE_HIGHLIGHTING" value="false" />
    <option name="TYPE_AWARE_HIGHLIGHTING_ENABLED" value="true" />
  </component>
</project>
"""
    writeToFileIfDifferent(file(ideaProjectRoot, "highlighting.xml"), highlightingContent)
  }

  def generateModule(project:Project) {
    val projComp = ProjectPhase(project, SourceCompilePhase)
    val projTestComp = ProjectPhase(project, TestCompilePhase)
    val sources = if (
      projComp.sourceDirs.isEmpty && 
      project.layout.resourceDirs.isEmpty && 
      project.layout.testResourceDirs.isEmpty && 
      projTestComp.sourceDirs.isEmpty) {
        """    <content url="file://$MODULE_DIR$" />"""
    } else {
      def sourceFolder(dir:File, test:Boolean=false) = {
        val relDir = dir.relativeTo(project.rootAbsoluteFile).getPath
        """      <sourceFolder url="file://$MODULE_DIR$/%s" isTestSource="%s" />""".format(relDir, test.toString)
      }
      val sourcesAndResources = (projComp.sourceDirs.filter(_.exists)
              ++ project.layout.resourceDirs.filter(_.exists)).map(sourceFolder(_, false))
      val testSourcesAndResources = (projTestComp.sourceDirs.filter(_.exists)
              ++ project.layout.testResourceDirs.filter(_.exists)).map(sourceFolder(_, true))
      val outputDirectoryToExclude = project.layout.targetDir.relativeTo(project.rootAbsoluteFile).getPath
      val allSources = (sourcesAndResources ++ testSourcesAndResources).toList.mkString("\n")
      """    <content url="file://$MODULE_DIR$">
%s
      <excludeFolder url="file://$MODULE_DIR$/.maker" />
      <excludeFolder url="file://$MODULE_DIR$/%s" />
    </content>""".format(allSources, outputDirectoryToExclude)
    }

    def libraryEntry(jarEntry:String, jarSourceEntry:String, provided:String) = """    <orderEntry type="module-library" exported=""%s>
      <library>
        <CLASSES>
%s
        </CLASSES>
        <JAVADOC />
%s
      </library>
    </orderEntry>""".format(provided, jarEntry, jarSourceEntry)

    def jarEntry(jarPath:String) = """          <root url="jar://$MODULE_DIR$/%s!/" />""".format(jarPath)
    def jarSourcesEntry(jarSourcesPath:Option[String]) = {
      jarSourcesPath match {
        case Some(jsp) => """        <SOURCES>
          <root url="jar://$MODULE_DIR$/%s!/" />
        </SOURCES>""".format(jsp)
        case None => """        <SOURCES />"""
      }
    }

    def libraryEntriesForJars(jarFiles:List[File], sourceFiles:List[File], provided:String) = {
      val jarFileToSourceFileMap = jarFiles.flatMap(jarFile => {
        sourceFiles.find(sourceFile => sourceFile.getName == jarFile.getName.replaceAll("\\.jar", "-sources.jar")) match {
          case Some(sf) => Some(jarFile -> sf)
          case _ => None
        }
      }).toMap
      val sortedJarFiles = jarFiles.sorted
      sortedJarFiles.map(jarFile => {
        val sourceFile = jarFileToSourceFileMap.get(jarFile)
        val jarPath = jarFile.relativeTo(project.rootAbsoluteFile).getPath
        val sourcePath = sourceFile.map(_.relativeTo(project.rootAbsoluteFile).getPath)
        val jarEntryString = jarEntry(jarPath)
        val jarSourcesEntryString = jarSourcesEntry(sourcePath)
        libraryEntry(jarEntryString, jarSourcesEntryString, provided)
      })
    }

    def jars(dirs:Set[File]) = dirs.flatMap(dir => {
      if (dir.exists) dir.listFiles.filter(_.getName.endsWith(".jar")).toList else Nil
    }).toList

    val managedLibraryJars = jars(Set(project.layout.managedLibDir))
    val managedLibrarySourceJars = jars(Set(project.layout.managedLibSourceDir))
    val managedLibraryEntries = libraryEntriesForJars(managedLibraryJars, managedLibrarySourceJars, "").mkString("", "\n", "\n")

    val unmanagedLibraryJars = jars(project.layout.unmanagedLibDirs).filterNot(_.getName.endsWith("-sources.jar"))
    val unmanagedLibraryEntries = libraryEntriesForJars(unmanagedLibraryJars, Nil, "").mkString("", "\n", "\n")

    val providedLibraryJars = jars(project.layout.providedLibDirs).filterNot(_.getName.endsWith("-sources.jar"))
    val providedLibraryEntries = libraryEntriesForJars(providedLibraryJars, Nil, """ scope="PROVIDED"""").mkString("\n")

    val moduleDependencies = {
      def moduleDependency(module:String, scope:String) =
        """    <orderEntry type="module" module-name="%s" exported="" %s/>""".format(module, scope)

      val dependencies = project.upstreamProjects.map(up         => moduleDependency(up.name, "")) ++
                         project.testOnlyUpStreamProjects.map(tp => moduleDependency(tp.name, """scope="TEST" """))

      dependencies.mkString("\n") + "\n"
    }

    val dependencies = moduleDependencies + managedLibraryEntries + unmanagedLibraryEntries + providedLibraryEntries

    val output = {
      val relativeOutputDir = projComp.outputDir.relativeTo(project.rootAbsoluteFile).getPath
      val relativeTestOutputDir = projTestComp.outputDir.relativeTo(project.rootAbsoluteFile).getPath
      """    <output url="file://$MODULE_DIR$/%s" />
    <output-test url="file://$MODULE_DIR$/%s" />""".format(relativeOutputDir, relativeTestOutputDir)
    }

    def resourcesDirContent(dir:String, test:String) = """    <orderEntry type="module-library" exported=""%s>
      <library>
        <CLASSES>
          <root url="file://$MODULE_DIR$/%s" />
        </CLASSES>
        <JAVADOC />
        <SOURCES />
      </library>
    </orderEntry>""".format(test, dir)

    val resourceDirectoriesThatExist = project.layout.resourceDirs.filter(_.exists)
    val resources = resourceDirectoriesThatExist.map(file => resourcesDirContent(file.relativeTo(project.rootAbsoluteFile).getPath, "")).toList
    val testResourceDirectoriesThatExist = project.layout.testResourceDirs.filter(_.exists)
    val testResources = testResourceDirectoriesThatExist.map(file => resourcesDirContent(file.relativeTo(project.rootAbsoluteFile).getPath, """ scope="TEST"""")).toList
    val resourceDirEntry = (resources ::: testResources).mkString("\n")

    val moduleContent = """<?xml version="1.0" encoding="UTF-8"?>
<module type="JAVA_MODULE" version="4">
  <component name="FacetManager">
    <facet type="scala" name="Scala">
      <configuration>
        <option name="compilerLibraryLevel" value="Project" />
        <option name="compilerLibraryName" value="scala-compiler-%s" />
        <option name="deprecationWarnings" value="true" />
        <option name="fsc" value="true" />
        <option name="uncheckedWarnings" value="true" />
      </configuration>
    </facet>
  </component>
  <component name="NewModuleRootManager" inherit-compiler-output="false">
%s
    <exclude-output />
%s
    <orderEntry type="inheritedJdk" />
    <orderEntry type="sourceFolder" forTests="false" />
    <orderEntry type="library" name="scala-library-%s" level="project" />
%s
%s
  </component>
</module>
                        """ % (scalaVersion, output, sources, scalaVersion, dependencies, resourceDirEntry)
    writeToFileIfDifferent(file(project.rootAbsoluteFile, project.name + ".iml"), moduleContent)
  }

  def generateModulesFile(ideaProjectRoot:File, modules:List[String]) {
    case class ModuleEntry(moduleName: String, isProject: Boolean = false) {
      def moduleFile = (if (isProject) moduleName else moduleName + "/" + moduleName) + ".iml"
      def xml = """      <module fileurl="file://$PROJECT_DIR$/%s" filepath="$PROJECT_DIR$/%s" />""".format(moduleFile, moduleFile)
    }
    val moduleEntries = (modules match {
      case head :: tail => {
        ModuleEntry(head, isProject = true) :: tail.map(ModuleEntry(_))
      }
      case _ => Nil
    }).sortBy(_.moduleName).map(_.xml).mkString("\n")

    val modulesContent = """<?xml version="1.0" encoding="UTF-8"?>
<project version="4">
  <component name="ProjectModuleManager">
    <modules>
%s
    </modules>
  </component>
</project>

""".format(moduleEntries)
    writeToFileIfDifferent(file(ideaProjectRoot, "modules.xml"), modulesContent)
  }

  private def writeToFileIfDifferent(file : File, text : String) = {
    def textDiffersMateriallyFromCurrentFile = {
      val currentLines = file.readLines.toArray.filter(_.trim.nonEmpty).map(_.trim).toList
      val textLines = text.split("\n").filter(_.trim.nonEmpty).map(_.trim).toList
      currentLines != textLines
    }
    if (! file.exists || textDiffersMateriallyFromCurrentFile)
        writeToFile(file, text)
    file
  }

  private def writeMiscFileIfRequired(file : File, text : String) = {
    def textContainsNewLines = {
      val currentLines = file.readLines.toArray.filter(_.trim.nonEmpty).map(_.trim).toSet
      val textLines = text.split("\n").filter(_.trim.nonEmpty).map(_.trim).toSet
      !textLines.subsetOf(currentLines)
    }
    if (! file.exists || textContainsNewLines) 
      writeToFile(file, text)
    file
  }
}
