package maker.project

import maker.task.BuildResult
import maker.task.tasks.{UpdateTask, MakePoms, CleanTask}
import maker.task.Build
import maker.task.compile.SourceCompileTask

/**
  * A collection of convenience methods for executing common tasks
  */
trait ProjectSugar {
  self : Project ⇒ 
    /**
     * Cleans this project and its upstream dependencies
     */
    def clean : BuildResult = Clean().execute

    /**
     * A more thorough clean - as well as cleaning class files it also deletes and managed libraries
     */
    def cleanAll : BuildResult = Clean(deleteManagedLibs = true).execute
    /**
     * Cleans just this project leaving upstream dependencies alone
     */
    def cleanOnly : BuildResult = Build.singleTask(new CleanTask(this)).execute

    def compile : BuildResult = Compile().execute
    def compileOnly : BuildResult = Build.singleTask(new SourceCompileTask(this)).execute
    def compileContinuously = continuously(() ⇒ Compile())

    def testCompile : BuildResult = TestCompile().execute
    def testCompileContinuously = continuously(() ⇒ TestCompile())

    def test = Test().execute
    def testNoCompile = TestNoCompile().execute
    def testOnly = TestOnly().execute
    def testClass(testClassName : String) =
      TestClass(testClassName).execute

    def testClassContinuously(testClassName : String) {
      // Run the test immediately without waiting for a source change
      TestClass(testClassName).execute
      continuously(() ⇒ TestClass(testClassName))
    }

    def testFailingSuites = RunFailingTests.execute

    def pack = PackageJar().execute

    def update : BuildResult = Update().execute
    def updateOnly : BuildResult = Build(new UpdateTask(this)).execute

    def publishLocal : BuildResult = PublishLocal().execute
    def publish : BuildResult = Publish().execute

    def runMain(className : String)(opts : String*)(args : String*) = RunMain(className)(opts : _*)(args : _*).execute
    def runMainContinuously(className : String)(opts : String*)(args : String*) =  continuously(() ⇒ RunMain(className)(opts : _*)(args : _*))

    /// doc task generates code documentation (scala-docs only at the moment)
    def doc : BuildResult = Doc().execute

    /// generates the maven poms to build each module
    // if invoked from a top level project a reactor top level pom will be created
    //   and the sub-module poms will have a parent pointing at the the top level project
    def makePoms : BuildResult = {
      val maybeTlp = if (this.isInstanceOf[TopLevelProject])
        Some(self.asInstanceOf[TopLevelProject])
      else
        None

      MakePoms(maybeParentProject = maybeTlp).execute
    }

}
