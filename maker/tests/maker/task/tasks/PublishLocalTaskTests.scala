package maker.task.tasks

import org.scalatest.{FreeSpec, Matchers, Assertions}
import maker.utils.FileUtils._
import maker.project._
import maker._
import org.apache.commons.io.{FileUtils => ApacheFileUtils}
import scala.xml.{XML, Node}
import maker.utils.{CustomMatchers, FileUtils}
import java.io.File
import scala.collection.immutable.Nil
import maker.task.compile.SourceCompilePhase
import org.eclipse.aether.graph.{Dependency => AetherDependency}

class PublishLocalTaskTests 
  extends FreeSpec 
  with Matchers 
  with CustomMatchers
  with FileUtils
  with DependencyPimps
  with Assertions
{

  "Single module project should publish as expected" in {
    withTempDir{
      rootDirectory =>  

        writeToFile(
          file(rootDirectory, "a/src/a/foo/Foo.scala"),
          """
            package foo
            case object Foo
          """
        )
        writeToFile(
          file(rootDirectory, "b/src/b/foo/Foo.scala"),
          """
            package bar
            case object Bar
          """
        )

        val organization = "org.org"
        TestMakerRepl.writeProjectFile(
          rootDirectory,
          s"""
            lazy val a = new Module(
              root = file("$rootDirectory", "a"),
              name = "a"
            ) with ClassicLayout {
              override def dependencies = Seq(
                "org.scalatest" % "scalatest" %% "2.2.0"
              )
            }

            lazy val b = new Module(
              root = file("$rootDirectory", "b"),
              name = "b",
              compileDependencies = Seq(a)
            ) with ClassicLayout 

            lazy val p = new Project(
              "p",
              file("$rootDirectory"),
              Seq(b),
              organization = Some("$organization")
            ) 
          """
        )
        val repl = TestMakerRepl(rootDirectory)
        val resourceDir = file(repl.value("a.resourceDir(SourceCompilePhase)"))
        val artifactId = repl.value("p.artifactId")
        file(resourceDir, "MainResource1").touch
        file(resourceDir, "subdir-b/MainResource2").touch

        val version = "1.0-SNAPSHOT"

        repl.inputLine(s"""p.publishLocal("$version", signArtifacts = false)""")

        val pomFile = file(repl.value(s"""p.publishLocalPomFile("$version")"""))
        val pom = XML.loadFile(pomFile)

        List(
          (organization, "groupId"), 
          (artifactId, "artifactId"),
          (version, "version")
        ).foreach{
          case (expected, label) => 
            assertResult(expected, s"label = $label"){(pom \ label).text}
        }
        val pomDependencies = pom \\ "dependency"
        val deps: Seq[RichDependency] = Seq(
          ScalaVersion.TWO_ELEVEN_DEFAULT.scalaLibraryRichDependency,
          "org.scalatest" % "scalatest" %% "2.2.0"
        ) 
        deps should allSatisfy {
          dep: RichDependency => 
            val aetherDep = dep.aetherDependency(ScalaVersion.TWO_ELEVEN_DEFAULT)
            pomDependencies.exists{
              node => 
                val pomCoords = Seq("groupId", "artifactId", "version").map{label => (node \ label).text}
                Seq(aetherDep.groupId, aetherDep.artifactId, aetherDep.version) === pomCoords
            }
        }

        val a_classDir = file(repl.value("a.classDirectory(SourceCompilePhase)"))
        val b_classDir = file(repl.value("b.classDirectory(SourceCompilePhase)"))
        val publishLocalJar = file(repl.value(s"""p.publishLocalJar("$version")"""))
        val a_sourceDir = file(repl.value("a.sourceDirs(SourceCompilePhase).head"))
        val b_sourceDir = file(repl.value("b.sourceDirs(SourceCompilePhase).head"))
        val sourceJar = file(repl.value(s"""p.sourcePackageJar(version = Some("$version"))"""))
        Seq(a_classDir, b_classDir).foreach {
          cd => 
            PackageJarTaskTests.checkJarContainsDirectoryContents(
              cd, publishLocalJar
            )
        }
        PackageJarTaskTests.checkJarContainsDirectoryContents(
          resourceDir, publishLocalJar
        )
        Seq(a_sourceDir, b_sourceDir).foreach {
          sd => 
            PackageJarTaskTests.checkJarContainsDirectoryContents(
              sd, sourceJar
            )
        }
    }
  }

}
