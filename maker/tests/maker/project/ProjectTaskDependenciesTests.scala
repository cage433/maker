package maker.project

import org.scalatest.{FunSuite, Matchers, FreeSpec}
import maker.task._
import maker.utils.Stopwatch
import java.io.File
import maker.task.tasks.{CleanTask, RunUnitTestsTask}
import maker.utils.FileUtils._
import maker.task.compile._
import maker.utils.os.Command
import maker.{ScalaVersion, TestMakerRepl}

class ProjectTaskDependenciesTests extends FreeSpec with Matchers {
  private val SCALA_VERSION = ScalaVersion.TWO_ELEVEN_DEFAULT

  "Can add custom upstream and downstream tasks" in {

    withTempDir{
      rootDirectory => 

        val markCompilationFile = file(rootDirectory, "countCompiles")
        val markCleanFile = file(rootDirectory, "countCleans")

        TestMakerRepl.writeProjectFile(
          rootDirectory,
          s"""
            import maker.task._
            import maker.task.tasks.CleanTask
            import maker.task.compile.CompileTask
            import maker.utils.Stopwatch
            import java.io.File

            case class TouchFile(name: String, path: String) extends Task with FileUtils {
              def exec(results: Iterable[TaskResult], sw: Stopwatch) = {
                file(path).touch
                DefaultTaskResult(this, succeeded = true, stopwatch = sw)
              }
              def upstreamTasks = Nil
            }
            lazy val a = new Module(
              root = file("$rootDirectory"),
              name = "a"
            ) with ClassicLayout {
              override def dependencies = Seq(
                "org.scalatest" % "scalatest" %%  "2.2.0"
              )
              override def extraUpstreamTasks(task: Task) = task match {
                case _ : CompileTask => 
                  Seq(TouchFile("Mark Compilation", "${markCompilationFile.getAbsolutePath}"))
                case _ => Nil
              }
              override def extraDownstreamTasks(task: Task) = task match {
                case _ : CleanTask => 
                  Seq(TouchFile("Mark Cleans", "${markCleanFile.getAbsolutePath}"))

                case _ => Nil
              }
            }

          """
        )
        val repl = TestMakerRepl(rootDirectory)
        markCompilationFile.exists should be (false)
        repl.inputLine("a.compile")
        markCompilationFile.exists should be (true)
        markCleanFile.exists should be (false)
        repl.inputLine("a.clean")
        markCleanFile.exists should be (true)

    }
  }


  "Module setUp and tearDown can be overriden" in {
    withTempDir{
      rootDirectory => 
        val markSetUpFile = file(rootDirectory, "markSetUp")
        val markTearDownFile = file(rootDirectory, "markTearDown")
        TestMakerRepl.writeProjectFile(
          rootDirectory,
          s"""
            import maker.task._
            import maker.task.tasks.CleanTask
            import maker.task.compile.CompileTask
            import maker.utils.Stopwatch
            import java.io.File

            lazy val a = new Module(
              root = file("$rootDirectory"),
              name = "a"
            ) with ClassicLayout with FileUtils {
              override def setUp(graph: Dependency.Graph) = {
                file("${markSetUpFile.getAbsolutePath}").touch
                super.setUp(graph)
              }
              override def tearDown(graph: Dependency.Graph, result: BuildResult) = {
                file("${markTearDownFile.getAbsolutePath}").touch
                super.tearDown(graph, result)
              }
            }

          """
        )
        val repl = TestMakerRepl(rootDirectory)
        markSetUpFile.exists should be (false)
        markTearDownFile.exists should be (false)
        repl.inputLine("a.compile")
        markSetUpFile.exists should be (true)
        markTearDownFile.exists should be (true)
    }
  }

  "TestCompile by default doesn't depend on upstream modules TestCompile" in {
    withTempDir{
      rootDirectory => 
        val A = new Module(file(rootDirectory, "A"), "A")
        val B = new Module(
          file(rootDirectory, "B"), 
          "B", 
          compileDependencies = Seq(A)
        )
        val C = new Module(file(rootDirectory, "C"), "C", 
          compileDependencies = Seq(A), 
          testDependencies = Seq(A)
        )

        assert(
          !B.compileTaskBuild(TestCompilePhase :: Nil).graph.upstreams(
            CompileTask(B, B, TestCompilePhase)
          ).contains(CompileTask(B, A, TestCompilePhase)),
          "Unless explicitly stated upstream test compilation is not a dependency"  
        )
        assert(
          C.compileTaskBuild(TestCompilePhase :: Nil).graph.upstreams(
            CompileTask(C, C, TestCompilePhase)
          ).contains(CompileTask(C, A, TestCompilePhase)),
          "When explicitly stated upstream test compilation is a dependency"  
        )
        assert(
          B.compileTaskBuild(SourceCompilePhase :: Nil).graph.upstreams(
            CompileTask(B, B, SourceCompilePhase)
          ).contains(CompileTask(B, A, SourceCompilePhase)),
          "Upstream source compilation is a dependency"  
        )

    }
  }

  "test dependencies are observed in classpaths" in {
    withTempDir{
      dir => 
        val A = new Module(file(dir, "A"), "A")
        val B = new Module(file(dir, "B"), "B", List(A))
        val C = new Module(file(dir, "C"), "C", List(A), List(A))
        val D = new Module(file(dir, "D"), "D", List(C))

        assert(
          ! B.compilationClasspathComponents(TestCompilePhase).contains(A.classDirectory(TestCompilePhase)), 
          "A's test output directory is not in B's classpath"
        )
        assert(
          C.compilationClasspathComponents(TestCompilePhase).contains(A.classDirectory(TestCompilePhase)), 
          "A's test output directory is in C's classpath"
        )
        assert(
          ! D.compilationClasspathComponents(TestCompilePhase).contains(A.classDirectory(TestCompilePhase)), 
          "A's test output directory is in D's classpath"
        )
    }
  }

  "Upstream module tests are associated tasks" in {
    withTempDir{
      dir => 
        def module(name : String, upstreams : List[Module] = Nil, testUpstreams : List[Module] = Nil) : Module = {
          new Module(file(dir, name), name, upstreams, testUpstreams)
        }
        val A = module("A")
        val B = module("B", List(A))
        val C = module("C", List(B), List(A))
        val D = module("D", List(C))

        List(A, B, C).foreach{
          proj => 
            assert(proj.testTaskBuild(TestCompilePhase, lastCompilationTimeFilter = None).graph.nodes.exists{
              case RunUnitTestsTask(_, _, `proj`, _, _) => true
              case _ => false
            })
        }
        import Dependency.Edge
        assert(!D.compileTaskBuild(TestCompilePhase :: Nil).graph.edges.contains(
          Edge(CompileTask(D, A, TestCompilePhase), CompileTask(D, C, TestCompilePhase))))

        assert(C.testTaskBuild(TestCompilePhase, lastCompilationTimeFilter = None).graph.edges.contains(
          Edge(CompileTask(C, A, TestCompilePhase), CompileTask(C, C, TestCompilePhase))))
        assert(!D.compileTaskBuild(TestCompilePhase :: Nil).graph.edges.contains(
          Edge(CompileTask(D, B, TestCompilePhase), CompileTask(D, C, TestCompilePhase))))

    }
    
  }
}
