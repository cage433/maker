package maker.project

import org.scalatest.FunSuite
import maker.task.Task
import maker.task.TaskResult
import maker.utils.Stopwatch
import maker.task.Dependency
import java.io.File
import maker.task.tasks.CleanTask
import maker.utils.FileUtils._
import maker.task.BuildResult
import maker.task.tasks.RunUnitTestsTask
import maker.task.compile._
import maker.task.SingleModuleTask
import maker.task.DefaultTaskResult

class ProjectTaskDependenciesTests extends FunSuite{
  case class WriteClassCountToFile(module : Module, basename : String = "ClassCount") 
    extends SingleModuleTask(module)
  {
    def name = "Write class count "
    def copy_(p : Module) = copy(module = p)
    def upstreamTasks = Nil
    def exec(results : Iterable[TaskResult] = Nil, sw : Stopwatch) : TaskResult = {
      exec
      DefaultTaskResult(WriteClassCountToFile.this, true, sw)
    }
    def exec = {
      writeToFile(file(module.rootAbsoluteFile, basename), module.compilePhase.classFiles.size + "")
    }
  }

  test("Can add custom task to run before standard task"){
    def moduleWithCustomTaskAfterClean(root : File, name : String) = new TestModule(root,name){
      self =>  
      val extraUpstreamTask = WriteClassCountToFile(this)
      override def extraUpstreamTasksMatcher = {
        case _ : CleanTask => Set(WriteClassCountToFile(self, "BeforeClean"))
      }
      override def extraDownstreamTasksMatcher = {
        case _ : CleanTask => Set(WriteClassCountToFile(self, "AfterClean"))
      }
    }

    withTempDir{
      dir => 
        val module = moduleWithCustomTaskAfterClean(dir, "CustomTask")
        module.writeSrc(
          "foo/Fred.scala",
          """
          |package foo
          |
          |case class Fred(i : Int)
          """
        )

        def classCountFile(basename : String) = file(module.rootAbsoluteFile, basename)
        assert(!classCountFile("BeforeClean").exists, "file should not exist until clean task is run")
        assert(!classCountFile("AfterClean").exists, "file should not exist until clean task is run")
        module.clean
        assert(classCountFile("BeforeClean").exists, "file should exist after clean task has run")
        assert(classCountFile("AfterClean").exists, "file should exist after clean task has run")

        def numberOfClassFiles(classCountFileBasename : String) : Int = {
          classCountFile(classCountFileBasename).readLines.toList.head.toInt
        }
        assert(numberOfClassFiles("BeforeClean") === 0)
        assert(numberOfClassFiles("AfterClean") === 0)
        module.compile
        module.clean
        assert(numberOfClassFiles("BeforeClean") > 0)
        assert(numberOfClassFiles("AfterClean") === 0)
    }
  }


  test("Module setUp and tearDown can be overriden"){
    def moduleWithSetupAndTeardowns(root : File, upstreamProjects : Module*) = new TestModule(
      root = root, 
      name = "With setup",
      upstreamProjects = upstreamProjects.toList
    ) {
      val setUpClassCountFile = file(rootAbsoluteFile, "setup")
      val tearDownClassCountFile= file(rootAbsoluteFile, "teardown")
      def graphContainsClean(graph : Dependency.Graph) = {
        graph.nodes.exists {
          case _ : CleanTask => true
          case _ => false
        }
      }
      override def setUp(graph : Dependency.Graph) = {
        if (graphContainsClean(graph))
          WriteClassCountToFile(this, "setup").exec
        super.setUp(graph)
      }
      override def tearDown(graph : Dependency.Graph, result : BuildResult) = {
        if (graphContainsClean(graph))
          WriteClassCountToFile(this, "teardown").exec
        super.tearDown(graph, result)
      }
      def deleteClassCountFiles{
        setUpClassCountFile.delete
        tearDownClassCountFile.delete
      }
      def setUpClassCount : Int = setUpClassCountFile.readLines.toList.headOption.map(_.toInt).getOrElse(0)
      def tearDownClassCount : Int = setUpClassCountFile.readLines.toList.headOption.map(_.toInt).getOrElse(0)
    }
    withTempDir{
      dir => 
        val upstreamModule = moduleWithSetupAndTeardowns(file(dir, "upstream"))
        val downstreamModule = moduleWithSetupAndTeardowns(file(dir, "downstream"))
        upstreamModule.writeSrc(
          "upstream/Foo",
          """
          |package upstream
          |
          |case class Foo(x : Int)
          """
        )
        downstreamModule.writeSrc(
          "downstream/Bar",
          """
          |package downstream
          |
          |import upstream.Foo
          |case class Bar(foo : Foo)
          """
        )
        // Initially there should be no class count files
        List(upstreamModule, downstreamModule).foreach{
          proj => 
            assert(!proj.setUpClassCountFile.exists)
            assert(!proj.tearDownClassCountFile.exists)
        }

        // After cleaning downstream its class count files only should exist
        downstreamModule.clean
        assert(!upstreamModule.setUpClassCountFile.exists)
        assert(!upstreamModule.tearDownClassCountFile.exists)
        assert(downstreamModule.setUpClassCountFile.exists)
        assert(downstreamModule.tearDownClassCountFile.exists)
    }
  }

  test("TestCompile by default doesn't depend on upstream modules TestCompile"){
    withTempDir{
      dir => 
        val A = new TestModule(file(dir, "upstream"), "A")
        val B = new TestModule(file(dir, "downstream"), "B", List(A))
        val C = new TestModule(file(dir, "downstream2"), "C", List(A), List(A))

        assert(
          !B.moduleBuild(TestCompileTask(_)).graph.upstreams(TestCompileTask(B)).contains(TestCompileTask(A)),
          "Unless explicitly stated upstream test compilation is not a dependency"  
        )
        assert(
          C.moduleBuild(TestCompileTask(_)).graph.upstreams(TestCompileTask(C)).contains(TestCompileTask(A)),
          "When explicitly stated upstream test compilation is a dependency"  
        )
        assert(
          B.moduleBuild(SourceCompileTask(_)).graph.upstreams(SourceCompileTask(B)).contains(SourceCompileTask(A)),
          "Upstream source compilation is a dependency"  
        )

    }
  }

  test("test dependencies are observed in classpaths"){
    withTempDir{
      dir => 
        val A = new TestModule(file(dir, "A"), "A")
        val B = new TestModule(file(dir, "B"), "B", List(A))
        val C = new TestModule(file(dir, "C"), "C", List(A), List(A))
        val D = new TestModule(file(dir, "D"), "D", List(C))

        assert(
          ! B.testCompilePhase.classpathDirectoriesAndJars.toSet.contains(A.testOutputDir), 
          "A's test output directory is not in B's classpath"
        )
        assert(
          C.testCompilePhase.classpathDirectoriesAndJars.toSet.contains(A.testOutputDir), 
          "A's test output directory is in C's classpath"
        )
        assert(
          ! D.testCompilePhase.classpathDirectoriesAndJars.toSet.contains(A.testOutputDir), 
          "A's test output directory is not in D's classpath"
        )
    }
  }

  test("Upstream module tests are associated tasks"){
    withTempDir{
      dir => 
        def module(name : String, upstreams : List[Module] = Nil, testUpstreams : List[Module] = Nil) : Module = {
          new TestModule(file(dir, name), name, upstreams, testUpstreams)
        }
        val A = module("A")
        val B = module("B", List(A))
        val C = module("C", List(B), List(A))
        val D = module("D", List(C))

        List(A, B, C).foreach{
          proj => 
            assert(proj.moduleBuild(RunUnitTestsTask(_, verbose = false)).graph.nodes.exists{
              case RunUnitTestsTask(_, `proj`, _, false) => true
              case _ => false
            })
        }
        import Dependency.Edge
        assert(!D.moduleBuild(TestCompileTask(_)).graph.edges.contains(Edge(TestCompileTask(A), TestCompileTask(C))))

        assert(D.testBuild(verbose = false).graph.edges.contains(Edge(TestCompileTask(A), TestCompileTask(C))))
        assert(!D.moduleBuild(TestCompileTask(_)).graph.edges.contains(Edge(TestCompileTask(B), TestCompileTask(C))))

        assert(D.moduleBuild(TestCompileTask(_)).graph.subGraphOf(D.test.graph))
        
    }
    
  }
}
