package maker.project

import maker.utils.FileUtils._
import maker.utils.MakerTestResults
import scalaz.Scalaz._
import java.io.File
import maker.MakerProps
import java.lang.reflect.Modifier
import maker.task.compile._
import maker.utils.RichIterable._

trait ProjectTestHelper{

  self : Project ⇒ 

    lazy val testOutputFile = file(rootAbsoluteFile, "maker-test-output")
    def testResultsOnly = MakerTestResults(props, testOutputFile)
    def testResults = allUpstreamProjects.map(_.testResultsOnly).reduce(_++_)
    /**
    * Convenience for unit tests
    */
    def sourceDirectory : File = layout.sourceDirs.uniqueElement
    def testDirectory : File = layout.testSourceDirs.uniqueElement

    /**
    * For debugging unit tests - writes a scala
    * project file
    */
    def writeMakerFile{
      val makerFile = if (upstreamProjects.isEmpty) 
        file(rootAbsoluteFile, "Maker.scala")
      else
        file(rootAbsoluteFile, "..", "Maker.scala")

      var lines : List[String] = Nil
      def buildLines(projects : Set[Project]){
        var line = "import maker.task.tasks._\n"
        line += "import maker.task._\n"
        line += "import maker.task.Dependency._\n"
        line += "import maker.project._\n"
        line += "import maker.utils.FileUtils._\n"
        projects.foreach{
          p ⇒ 
            line += "\n\n"
            line += "val " + p.name + " : Project = {" +
              "\n\tval root = new java.io.File(\"" + p.rootAbsoluteFile + "\")\n" + 
              "\n\tnew TestProject(root, \"" + p.name + "\", Nil, List("
              line += p.upstreamProjects.map(_.name).mkString(", ") + "))"
            line +="\n}"
            lines = line :: lines
        }
        projects.flatMap(_.upstreamProjects) |> {
          nextProjects ⇒ 
            if (!nextProjects.isEmpty)
              buildLines(nextProjects)
        }
      }
      buildLines(Set(this))
      writeToFile(
        makerFile,
        "import maker.project._\nimport java.io.File\n\n" + lines.mkString("", "\n", "\n")
      )
    }

    /**
      * To run tests from Vim it is convenient to have _all_ test classes on the classpath,
      * Not just those projects on whom we have a test dependency
      */
    def writeVimClasspath {
      var dirsAndJars = testCompilePhase.classpathDirectoriesAndJars.toList
      dirsAndJars ::= props.ScalaCompilerJar()
      dirsAndJars ::= props.ScalaLibraryJar()
      val cp = Project.asClasspathStr(dirsAndJars)
      val cpFile : File = file(name + "-classpath.sh")
      writeToFile(cpFile, "export CLASSPATH=" + cp + "\n")
      appendToFile(cpFile, "export JAVA_OPTS=\" " + props.MakerHome.toCommandLine + " \"\n")
    }


    def delete = recursiveDelete(rootAbsoluteFile)
}
