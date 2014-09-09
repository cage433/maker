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
import maker.task.compile._
import maker.utils.RichString._
import maker.MakerProps

case class IDEAProjectGenerator(props : MakerProps) {
  val scalaVersion = props.ProjectScalaVersion()

  def generateTopLevelModule(rootDir:File, name:String, excludedFolders:List[String]) {

    val formattedExcludedFolders = if (excludedFolders.nonEmpty) {
      val excludedFoldersString = excludedFolders.sorted.map(folder => {
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
  <component name="NewModuleRootManager" inherit-compiler-output="false">
    <output url="file://$MODULE_DIR$/never-created-classes" />
    <output-test url="file://$MODULE_DIR$/never-created-test-classes" />
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
    val ideaProjectRoot = mkdir(file(rootDir, ".idea"))
    writeToFileIfDifferent(file(ideaProjectRoot, ".name"), name)

    // For now we are insisting that there is a "scala directory" at this location: $PROJECT_DIR$/lib/scala/lib_managed/
    // Obviously, this isn't very good and we should allow it to be specified.
    List(props.ProjectScalaLibraryJar(), props.ProjectScalaCompilerJar()).foreach{
      jar => 
      if (!jar.exists) {
        throw new Exception("Maker requires that file " + jar + " + be present. Launch maker with the '-y' flag to download it")
      }
    }

    val librariesDir = mkdir(file(ideaProjectRoot, "libraries"))
    val scalaLibraryContent = """<component name="libraryTable">
  <library name="scala-library-%s">
    <CLASSES>
      <root url="jar://$PROJECT_DIR$/%s!/" />
    </CLASSES>
    <JAVADOC />
    <SOURCES>
      <root url="jar://$PROJECT_DIR$/%s!/" />
    </SOURCES>
  </library>
</component>
""" % (scalaVersion, props.ProjectScalaLibraryJar().getPath, props.ProjectScalaLibrarySourceJar())
    writeToFileIfDifferent(file(librariesDir, "scala_library_%s.xml" % scalaVersion.replace('.', '_')), scalaLibraryContent)

    val scalaCompilerLibraryContent = """<component name="libraryTable">
  <library name="scala-compiler-%s">
    <CLASSES>
      <root url="jar://$PROJECT_DIR$/%s!/" />
      <root url="jar://$PROJECT_DIR$/%s!/" />
    </CLASSES>
    <JAVADOC />
    <SOURCES />
  </library>
</component>
""" % (scalaVersion, props.ProjectScalaCompilerJar(), props.ProjectScalaLibraryJar())
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
<module version="4">
  <component name="ProjectRootManager" version="2" languageLevel="JDK_1_6" assert-keyword="true" jdk-15="true" project-jdk-name="JDK6" project-jdk-type="JavaSDK">
    <output url="file://$PROJECT_DIR$/out" />
  </component>
</module>
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

  def generateModule(module:Module) {
    val projComp = ModuleCompilePhase(module, SourceCompilePhase)
    val projTestComp = ModuleCompilePhase(module, TestCompilePhase)
    val sources = if (
      projComp.sourceDirs.map(_.exists).isEmpty &&
      !module.resourceDir.exists &&
      !module.managedResourceDir.exists &&
      !module.testResourceDir.exists &&
      projTestComp.sourceDirs.map(_.exists).isEmpty) {
        """    <content url="file://$MODULE_DIR$" />"""
    } else {
      def sourceFolder(dir:File, test:Boolean=false) = {
        val relDir = dir.relativeTo(module.rootAbsoluteFile).getPath
        """      <sourceFolder url="file://$MODULE_DIR$/%s" isTestSource="%s" />""".format(relDir, test.toString)
      }
      // see note on 'resourcesDirContent' about why we don't use 'java-resources'
      val sourcesAndResources = (projComp.sourceDirs.toSet.filter(_.exists)
              ++ Set(module.resourceDir, module.managedResourceDir).filter(_.exists)).map(sourceFolder(_, test = false))
      val testSourcesAndResources = (projTestComp.sourceDirs.toSet.filter(_.exists)
              ++ Set(module.testResourceDir).filter(_.exists)).map(sourceFolder(_, test = true))
      val outputDirectoryToExclude = module.targetDir.relativeTo(module.rootAbsoluteFile).getPath
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
        val jarPath = jarFile.relativeTo(module.rootAbsoluteFile).getPath
        val sourcePath = sourceFile.map(_.relativeTo(module.rootAbsoluteFile).getPath)
        val jarEntryString = jarEntry(jarPath)
        val jarSourcesEntryString = jarSourcesEntry(sourcePath)
        libraryEntry(jarEntryString, jarSourcesEntryString, provided)
      })
    }

    def jars(dirs:Iterable[File]): List[File] = for {
      dir <- dirs.toList
      files <- Option(dir.listFiles).toList // java null awesomeness
      file <- files
      if file.getName.endsWith(".jar")
    } yield file

    val managedLibraryJars = jars(List(module.managedLibDir))
    val managedLibrarySourceJars = managedLibraryJars.map{f =>
      // ensures that we create an entry for the source, even if we don't have it locally
      val dir = new File(f.getParentFile.getCanonicalPath.replace("/lib_managed", "/lib_src_managed"))
      new File(dir, f.getName.replace(".jar", "-sources.jar"))
    }
    val managedLibraryEntries = libraryEntriesForJars(managedLibraryJars, managedLibrarySourceJars, "").mkString("", "\n", "\n")

    val unmanagedLibraryJars = jars(module.unmanagedLibDirs).filterNot(_.getName.endsWith("-sources.jar"))
    val unmanagedLibraryEntries = libraryEntriesForJars(unmanagedLibraryJars, Nil, "").mkString("", "\n", "\n")

    val moduleDependencies = {
      def moduleDependency(module:String, scope:String) =
        """    <orderEntry type="module" module-name="%s" exported="" %s/>""".format(module, scope)

      val testOnlyUpStreamModules : List[Module] = {
        val allUpstream = module.allUpstreamModules

        module.immediateUpstreamTestModules.filterNot(tp => allUpstream.contains(tp))
      }

      val dependencies = module.immediateUpstreamModules.map(up         => moduleDependency(up.name, "")) ++
                         testOnlyUpStreamModules.map(tp => moduleDependency(tp.name, """scope="TEST" """))

      dependencies.mkString("\n") + "\n"
    }

    val dependencies = moduleDependencies + managedLibraryEntries + unmanagedLibraryEntries 

    val output = {
      val relativeOutputDir = projComp.outputDir.relativeTo(module.rootAbsoluteFile).getPath
      val relativeTestOutputDir = projTestComp.outputDir.relativeTo(module.rootAbsoluteFile).getPath
      """    <output url="file://$MODULE_DIR$/%s" />
    <output-test url="file://$MODULE_DIR$/%s" />""".format(relativeOutputDir, relativeTestOutputDir)
    }

    /*
     IntelliJ 13 added support for java-resource roots, but
     unfortunately all files are copied into the target directory. In
     order to agree with maker we don't want to copy any resources,
     but instead want them added to the classpath. So we do this hack
     AND add all resource roots as source roots AND explicitly exclude
     copying any resources from the source roots to the target.
     */
    def resourcesDirContent(dir:String, test:String) = """    <orderEntry type="module-library" exported=""%s>
      <library>
        <CLASSES>
          <root url="file://$MODULE_DIR$/%s" />
        </CLASSES>
        <JAVADOC />
        <SOURCES />
      </library>
    </orderEntry>""".format(test, dir)

    val resourceDirectoriesThatExist = Set(module.resourceDir, module.managedResourceDir).filter(_.exists)
    val resources = resourceDirectoriesThatExist.map(file => resourcesDirContent(file.relativeTo(module.rootAbsoluteFile).getPath, "")).toList
    val testResourceDirectoriesThatExist = Set(module.testResourceDir).filter(_.exists)
    val testResources = testResourceDirectoriesThatExist.map(file => resourcesDirContent(file.relativeTo(module.rootAbsoluteFile).getPath, """ scope="TEST"""")).toList
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
    writeToFileIfDifferent(file(module.rootAbsoluteFile, module.name + ".iml"), moduleContent)
  }

  def generateModulesFile(ideaProjectRoot:File, topLevel : Project) {
    def xml(p : BaseProject) = {
      val imlFile : File = p match {
        case _ : Module => file(p.name, p.name + ".iml")
        case _ : Project => file(p.name + ".iml")
      }
      """      <module fileurl="file://$PROJECT_DIR$/%s" filepath="$PROJECT_DIR$/%s" />""".format(imlFile, imlFile)
    }
    val moduleEntries = (topLevel :: topLevel.allModules.toList).sortBy(_.name).map(xml(_)).mkString("\n")

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

  @annotation.tailrec
  private def cleanWhitespace(s: String): String =
    s.trim.replace("\n\n", "\n") match {
      case clean if clean == s => s
      case clean => cleanWhitespace(clean)
    }

  private def writeToFileIfDifferent(file : File, text : String) = {
    def textDiffersMateriallyFromCurrentFile = {
      val currentLines = file.readLines.toArray.filter(_.trim.nonEmpty).map(_.trim).toList
      val textLines = text.split("\n").filter(_.trim.nonEmpty).map(_.trim).toList
      currentLines != textLines
    }
    if (! file.exists || textDiffersMateriallyFromCurrentFile)
        writeToFile(file, cleanWhitespace(text) + "\n\n")
    file
  }

  private def writeMiscFileIfRequired(file : File, text : String) = {
    def textContainsNewLines = {
      val currentLines = file.readLines.toArray.filter(_.trim.nonEmpty).map(_.trim).toSet
      val textLines = text.split("\n").filter(_.trim.nonEmpty).map(_.trim).toSet
      !textLines.subsetOf(currentLines)
    }
    if (! file.exists || textContainsNewLines) 
      writeToFile(file, cleanWhitespace(text) + "\n\n")
    file
  }
}
