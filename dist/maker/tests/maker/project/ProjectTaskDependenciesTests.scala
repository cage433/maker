package maker.project

import org.scalatest.FunSuite
import maker.task.Task
import maker.task.TaskResult
import maker.utils.Stopwatch
import maker.task.Dependency
import java.io.File
import maker.task.tasks.CleanTask
import maker.utils.FileUtils._
import maker.MakerProps
import maker.task.BuildResult
import maker.task.tasks.RunUnitTestsTask
import maker.task.compile._
import maker.task.Build

class ProjectTaskDependenciesTests extends FunSuite{
  case class WriteClassCountToFile(project : Project, basename : String = "ClassCount") extends Task{
    def name = "Write class count "
    def copy_(p : Project) = copy(project = p)
    def upstreamTasks = Nil
    def exec(results : List[TaskResult] = Nil, sw : Stopwatch) : TaskResult = {
      exec
      TaskResult.success(WriteClassCountToFile.this, sw)
    }
    def exec = {
      writeToFile(file(project.rootAbsoluteFile, basename), project.compilePhase.classFiles.size + "")
    }
  }

  test("Can add custom task to run before standard task"){
    def projectWithCustomTaskAfterClean(root : File, name : String) = new TestProject(root,name){
      self ⇒  
      val extraUpstreamTask = WriteClassCountToFile(this)
      override def extraUpstreamTasks(task : Task) = task match {
        case _ : CleanTask ⇒ Set(WriteClassCountToFile(self, "BeforeClean"))
        case _ ⇒ Set.empty
      }
      override def extraDownstreamTasks(task : Task) = task match {
        case _ : CleanTask ⇒ Set(WriteClassCountToFile(self, "AfterClean"))
        case _ ⇒ Set.empty
      }
    }

    withTempDir{
      dir ⇒ 
        val project = projectWithCustomTaskAfterClean(dir, "CustomTask")
        project.writeSrc(
          "foo/Fred.scala",
          """
          |package foo
          |
          |case class Fred(i : Int)
          """
        )

        def classCountFile(basename : String) = file(project.rootAbsoluteFile, basename)
        assert(!classCountFile("BeforeClean").exists, "file should not exist until clean task is run")
        assert(!classCountFile("AfterClean").exists, "file should not exist until clean task is run")
        project.clean
        assert(classCountFile("BeforeClean").exists, "file should exist after clean task has run")
        assert(classCountFile("AfterClean").exists, "file should exist after clean task has run")

        def numberOfClassFiles(classCountFileBasename : String) : Int = {
          classCountFile(classCountFileBasename).readLines.toList.head.toInt
        }
        assert(numberOfClassFiles("BeforeClean") === 0)
        assert(numberOfClassFiles("AfterClean") === 0)
        project.compile
        project.clean
        assert(numberOfClassFiles("BeforeClean") > 0)
        assert(numberOfClassFiles("AfterClean") === 0)
    }
  }


  test("Project setUp and tearDown can be overriden"){
    def projectWithSetupAndTeardowns(root : File, upstreamProjects : Project*) = new TestProject(
      root = root, 
      name = "With setup",
      upstreamProjects = upstreamProjects.toList
    ) {
      val setUpClassCountFile = file(rootAbsoluteFile, "setup")
      val tearDownClassCountFile= file(rootAbsoluteFile, "teardown")
      def graphContainsClean(graph : Dependency.Graph) = {
        graph.nodes.exists {
          case _ : CleanTask ⇒ true
          case _ ⇒ false
        }
      }
      override def setUp(graph : Dependency.Graph){
        if (graphContainsClean(graph))
          WriteClassCountToFile(this, "setup").exec
        super.setUp(graph)
      }
      override def tearDown(graph : Dependency.Graph, result : BuildResult){
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
      dir ⇒ 
        val upstreamProject = projectWithSetupAndTeardowns(file(dir, "upstream"))
        val downstreamProject = projectWithSetupAndTeardowns(file(dir, "downstream"))
        upstreamProject.writeSrc(
          "upstream/Foo",
          """
          |package upstream
          |
          |case class Foo(x : Int)
          """
        )
        downstreamProject.writeSrc(
          "downstream/Bar",
          """
          |package downstream
          |
          |import upstream.Foo
          |case class Bar(foo : Foo)
          """
        )
        // Initially there should be no class count files
        List(upstreamProject, downstreamProject).foreach{
          proj ⇒ 
            assert(!proj.setUpClassCountFile.exists)
            assert(!proj.tearDownClassCountFile.exists)
        }

        // After cleaning downstream its class count files only should exist
        downstreamProject.clean
        assert(!upstreamProject.setUpClassCountFile.exists)
        assert(!upstreamProject.tearDownClassCountFile.exists)
        assert(downstreamProject.setUpClassCountFile.exists)
        assert(downstreamProject.tearDownClassCountFile.exists)
    }
  }

  test("TestCompile by default doesn't depend on upstream projects TestCompile"){
    withTempDir{
      dir ⇒ 
        val A = new TestProject(file(dir, "upstream"), "A")
        val B = new TestProject(file(dir, "downstream"), "B", List(A))
        val C = new TestProject(file(dir, "downstream2"), "C", List(A), List(A))

        assert(
          !B.TestCompile.graph.upstreams(TestCompileTask(B)).contains(TestCompileTask(A)),
          "Unless explicitly stated upstream test compilation is not a dependency"  
        )
        assert(
          C.TestCompile.graph.upstreams(TestCompileTask(C)).contains(TestCompileTask(A)),
          "When explicitly stated upstream test compilation is a dependency"  
        )
        assert(
          B.Compile.graph.upstreams(SourceCompileTask(B)).contains(SourceCompileTask(A)),
          "Upstream source compilation is a dependency"  
        )

    }
  }

  test("test dependencies are observed in classpaths"){
    withTempDir{
      dir ⇒ 
        val A = new TestProject(file(dir, "A"), "A")
        val B = new TestProject(file(dir, "B"), "B", List(A))
        val C = new TestProject(file(dir, "C"), "C", List(A), List(A))
        val D = new TestProject(file(dir, "D"), "D", List(C))

        assert(
          ! B.testCompilePhase.classpathDirectoriesAndJars.contains(A.layout.testOutputDir), 
          "A's test output directory is not in B's classpath"
        )
        assert(
          C.testCompilePhase.classpathDirectoriesAndJars.contains(A.layout.testOutputDir), 
          "A's test output directory is in C's classpath"
        )
        assert(
          ! D.testCompilePhase.classpathDirectoriesAndJars.contains(A.layout.testOutputDir), 
          "A's test output directory is not in D's classpath"
        )
    }
  }

  test("Upstream project tests are associated tasks"){
    withTempDir{
      dir ⇒ 
        def project(name : String, upstreams : List[Project] = Nil, testUpstreams : List[Project] = Nil) : Project = {
          new TestProject(file(dir, name), name, upstreams, testUpstreams)
        }
        val A = project("A")
        val B = project("B", List(A))
        val C = project("C", List(B), List(A))
        val D = project("D", List(C))

        List(A, B, C).foreach{
          proj ⇒ 
            assert(proj.Test.graph.nodes.contains(RunUnitTestsTask(proj)))
        }
        import Dependency.Edge
        assert(!D.TestCompile.graph.edges.contains(Edge(TestCompileTask(A), TestCompileTask(C))))
        assert(D.Test.graph.edges.contains(Edge(TestCompileTask(A), TestCompileTask(C))))
        assert(!D.TestCompile.graph.edges.contains(Edge(TestCompileTask(B), TestCompileTask(C))))

        assert(D.TestCompile.graph.subGraphOf(D.Test.graph))
        
    }
    
  }
}
