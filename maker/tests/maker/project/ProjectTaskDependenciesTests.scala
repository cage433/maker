package maker.project

import org.scalatest.{FunSuite, Matchers}
import maker.task._
import maker.utils.Stopwatch
import java.io.File
import maker.task.tasks.{CleanTask, RunUnitTestsTask}
import maker.utils.FileUtils._
import maker.task.compile._
import maker.utils.os.Command
import maker.ScalaVersion

class ProjectTaskDependenciesTests extends FunSuite with Matchers with ModuleTestPimps{
  private val SCALA_VERSION = ScalaVersion.TWO_ELEVEN_DEFAULT

  ignore("Can add custom task to run before standard task"){

    withTempDir{
      dir => 
        TestModuleBuilder.createMakerProjectFile(dir)
        val bldr = new TestModuleBuilder(dir, "CustomUpstreamTask").withExtraCode(
          s"""|
              | import maker.task.tasks.CleanTask
              | import maker.task._
              | import maker.utils.FileUtils._
              | import maker.utils.Stopwatch
              | case class WriteClassCountToFile(basename : String) extends Task {
              |   def name = "Write class count "
              |   def upstreamTasks = Nil
              |   def exec(results : Iterable[TaskResult] = Nil, sw : Stopwatch) : TaskResult = {
              |     exec
              |     DefaultTaskResult(WriteClassCountToFile.this, true, sw)
              |   }
              |   def exec = {
              |     writeToFile(file(rootAbsoluteFile, basename), compilePhase.classFiles.size + "")
              |   }
              | }
              |
              | override def extraUpstreamTasksMatcher = {
              |   case _ : CleanTask => Set(WriteClassCountToFile("BeforeClean"))
              | }
              | override def extraDownstreamTasksMatcher = {
              |   case _ : CleanTask => Set(WriteClassCountToFile("AfterClean"))
              | }
              |
              | def numberOfClassFiles(classCountFileBasename : String) : Int = {
              |   classCountFile(classCountFileBasename).readLines.toList.head.toInt
              | }
              |
              | def classCountFile(basename : String) = file(basename)
              |
              | def checkClassCounts{
              |   assert(!classCountFile("BeforeClean").exists, "file should not exist until clean task is run")
              |   assert(!classCountFile("AfterClean").exists, "file should not exist until clean task is run")
              |   clean
              |   assert(classCountFile("BeforeClean").exists, "file should exist after clean task has run")
              |   assert(classCountFile("AfterClean").exists, "file should exist after clean task has run")
              |   assert(numberOfClassFiles("BeforeClean") == 0, "No class files at start")
              |   assert(numberOfClassFiles("AfterClean") == 0, "No class count in After at start")
              |   compile
              |   clean
              |   assert(numberOfClassFiles("BeforeClean") > 0, "Class count before clean")
              |   assert(numberOfClassFiles("AfterClean") == 0, "class count after clean")
              | }
              |
              |""".stripMargin
        )

        bldr.appendDefinitionToProjectFile(dir)
        bldr.writeSrc(
          "foo/Fred.scala",
          """
          |package foo
          |
          |case class Fred(i : Int)
          """
        )

        // sanity check that it runs
        file(dir, "BeforeClean").exists should be (false)

        val result = TestModuleBuilder.makerExecuteCommand(
          dir,
          "CustomUpstreamTask.checkClassCounts"
        ).withNoOutput.run

        result should equal (0)

        // sanity check that it ran
        file(dir, "BeforeClean").exists should be (true)
    }
  }


  test("Module setUp and tearDown can be overriden"){
    withTempDir{
      projectRoot => 
        TestModuleBuilder.createMakerProjectFile(projectRoot)
        def moduleBuilder(moduleName : String, upstreams : String*) = 
          new TestModuleBuilder(file(projectRoot, moduleName), moduleName, immediateUpstreamModuleNames = upstreams).withExtraCode(
          s"""|
              |
              | import maker.task.{Dependency, BuildResult}
              | import maker.task.tasks.CleanTask
              |
              | val setUpClassCountFile = file(rootAbsoluteFile, "setup")
              | val tearDownClassCountFile= file(rootAbsoluteFile, "teardown")
              | def graphContainsClean(graph : Dependency.Graph) = {
              |   graph.nodes.exists {
              |     case _ : CleanTask => true
              |     case _ => false
              |   }
              | }
              | override def setUp(graph : Dependency.Graph) = {
              |   if (graphContainsClean(graph))
              |     writeToFile(setUpClassCountFile, classFiles(defaultScalaVersion).size + "")
              |   super.setUp(graph)
              | }
              | override def tearDown(graph : Dependency.Graph, result : BuildResult) = {
              |   if (graphContainsClean(graph))
              |     writeToFile(tearDownClassCountFile, classFiles(defaultScalaVersion).size + "")
              |   super.tearDown(graph, result)
              | }
              | def deleteClassCountFiles{
              |   setUpClassCountFile.delete
              |   tearDownClassCountFile.delete
              | }
              | def setUpClassCount : Int = setUpClassCountFile.readLines.toList.headOption.map(_.toInt).getOrElse(0)
              | def tearDownClassCount : Int = setUpClassCountFile.readLines.toList.headOption.map(_.toInt).getOrElse(0)
              |""".stripMargin
        )

        val upstreamModule = moduleBuilder("upstream")
        val downstreamModule = moduleBuilder("downstream", "upstream")
        upstreamModule.appendDefinitionToProjectFile(projectRoot)
        downstreamModule.appendDefinitionToProjectFile(projectRoot)
        TestModuleBuilder.appendTopLevelProjectDefinition(
          projectRoot, "project", "upstream" :: Nil,
          extraCode = """|  def checkSetupTearDown{
                         |    List(upstream, downstream).foreach{
                         |      module => 
                         |        assert(!module.setUpClassCountFile.exists, s"$module should have no setup file")
                         |        assert(!module.tearDownClassCountFile.exists, s"$module should have no tearDown file")
                         |    }
                         |
                         |    // After cleaning downstream its class count files only should exist
                         |    downstream.clean
                         |    assert(!upstream.setUpClassCountFile.exists, "test 1")
                         |    assert(!upstream.tearDownClassCountFile.exists, "test 2")
                         |    assert(downstream.setUpClassCountFile.exists, "test 3")
                         |    assert(downstream.tearDownClassCountFile.exists, "test 4")
                         |}""".stripMargin

        )
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

        // Sanity check something executed
        file(projectRoot, "upstream", "setup").exists should be (false)
        file(projectRoot, "downstream", "setup").exists should be (false)

        val command = TestModuleBuilder.makerExecuteCommand(projectRoot, "project.checkSetupTearDown").withNoOutput
        command.run should equal(0)

        // Sanity check something executed
        file(projectRoot, "upstream", "setup").exists should be (false)
        file(projectRoot, "downstream", "setup").exists should be (true)
    }
  }

  ignore("TestCompile by default doesn't depend on upstream modules TestCompile"){
    withTempDir{
      dir => 
        val A = new TestModule(file(dir, "upstream"), "A")
        val B = new TestModule(file(dir, "downstream"), "B", List(A))
        val C = new TestModule(file(dir, "downstream2"), "C", List(A), List(A))

        assert(
          !B.testCompileTaskBuild(TestCompilePhase :: Nil).graph.upstreams(
            CompileTask(B, B, TestCompilePhase)
          ).contains(CompileTask(B, A, TestCompilePhase)),
          "Unless explicitly stated upstream test compilation is not a dependency"  
        )
        assert(
          C.testCompileTaskBuild(TestCompilePhase :: Nil).graph.upstreams(
            CompileTask(C, C, TestCompilePhase)
          ).contains(CompileTask(C, A, TestCompilePhase)),
          "When explicitly stated upstream test compilation is a dependency"  
        )
        assert(
          B.compileTaskBuild.graph.upstreams(
            CompileTask(B, B, SourceCompilePhase)
          ).contains(CompileTask(B, A, SourceCompilePhase)),
          "Upstream source compilation is a dependency"  
        )

    }
  }

  ignore("test dependencies are observed in classpaths"){
    withTempDir{
      dir => 
        val A = new TestModule(file(dir, "A"), "A")
        val B = new TestModule(file(dir, "B"), "B", List(A))
        val C = new TestModule(file(dir, "C"), "C", List(A), List(A))
        val D = new TestModule(file(dir, "D"), "D", List(C))

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

  ignore("Upstream module tests are associated tasks"){
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
            assert(proj.testTaskBuild(lastCompilationTimeFilter = None).graph.nodes.exists{
              case RunUnitTestsTask(_, _, `proj`, _, _, _) => true
              case _ => false
            })
        }
        import Dependency.Edge
        assert(!D.testCompileTaskBuild(TestCompilePhase :: Nil).graph.edges.contains(
          Edge(CompileTask(D, A, TestCompilePhase), CompileTask(D, C, TestCompilePhase))))

        assert(C.testTaskBuild(lastCompilationTimeFilter = None).graph.edges.contains(
          Edge(CompileTask(C, A, TestCompilePhase), CompileTask(C, C, TestCompilePhase))))
        assert(!D.testCompileTaskBuild(TestCompilePhase :: Nil).graph.edges.contains(
          Edge(CompileTask(D, B, TestCompilePhase), CompileTask(D, C, TestCompilePhase))))

    }
    
  }
}
