package maker.project
import maker.task._
import maker.task.tasks._
import maker.utils.StringUtils
import maker.utils.TestIdentifier
import maker.utils.TestFailure
import maker.utils.RichIterable._
import maker.MakerProps
import maker.utils.FileUtils
import java.io.File
import maker.task.compile.TestCompileTask
import maker.task.compile.SourceCompileTask
import maker.task.compile._

trait ProjectTasks{
  self : Project ⇒ 

    def Clean(deleteManagedLibs : Boolean = false) = Build(CleanTask(self, deleteManagedLibs))
    def Compile() = Build(SourceCompileTask(self))
    def TestCompile() = Build(TestCompileTask(self))
    def Test() = Build(
      RunUnitTestsTask(this),
      allUpstreamProjects.map{project : Project ⇒ 
        Dependency.Graph.transitiveClosure(RunUnitTestsTask(project))
      }.reduce(_++_)
    )
    def TestOnly() = Build(
      RunUnitTestsTask(this),
      Dependency.Graph.transitiveClosure(RunUnitTestsTask(this))
    )
    def TestNoCompile() = Build(
      RunUnitTestsTask(this),
      Dependency.Graph(allUpstreamProjects.map(RunUnitTestsTask(_)).toSet, Set.empty)
    )
      
        

    def TestClass(className : String) = {
      val actualClassName = if (className.contains('.'))
        Some(className)
      else {
        val matchingTestClasses = StringUtils.bestIntellijMatches(
          className, 
          testCompilePhase.fullyQualifiedClasses.filterNot(_.contains('$')).filter(isAccessibleScalaTestSuite)
        )
        if (matchingTestClasses.isEmpty){
          log.warn("No class matching " + className + " found")
          None
        } else {
          if (matchingTestClasses.size > 1)
            log.info("Multiple matches: " + matchingTestClasses.mkString(", ") + ", using " + matchingTestClasses.head)
          Some(matchingTestClasses.head)
        }
      }
      actualClassName.map{name ⇒ Build(RunUnitTestsTask(self, name))}.getOrElse(Build(NullTask))
    }

    def RunFailingTests() = Build(RunFailingTestsTask(this))

    def PackageJar(aggregateDependentModules : Boolean = false, includeDependentLibs : Boolean = false) = Build(PackageJarTask(self, aggregateDependentModules, includeDependentLibs))

    def Update(withSources : Boolean = true) = Build(UpdateTask(self, withSources, List("default")))
    def Update(withSources : Boolean, config : String, moreConfigs : String*) = Build(UpdateTask(self, withSources, config :: moreConfigs.toList))


    def PublishLocal(configurations : List[String] = List("default"), version : String = props.Version()) = Build(PublishLocalTask(this, configurations, version))

    def Publish(resolver : String = props.DefaultPublishResolver(), version : String = props.Version()) = Build(PublishTask(self, resolver, version))

    /**
    * Pass in a fully qualified class name that is in scope to this project's (and upstream) classpath
    *   - opts are set as JVM -D args to the process runner
    *   - args are passed to the main method
    */
    def RunMain(className : String)(opts : String*)(args : String*) = Build(RunMainTask(self, className, opts.toList, args.toList))


    def Doc(aggregate : Boolean = true) = Build(DocTask(this, aggregate))

    def MakePoms(configurations : List[String] = List("default"), version : String = props.Version(), maybeParentProject : Option[TopLevelProject] = None) =
      Build(new MakePoms(this, configurations, version, maybeParentProject))

    def continuously(bld : () ⇒ Build){
      var lastTaskTime :Option[Long] = None

      def allSourceFiles : List[File] = allUpstreamProjects.flatMap{
        proj ⇒ 
          compilePhase.sourceFiles++ testCompilePhase.sourceFiles
      }

      def sourceFileCount : Int = allSourceFiles.size
      var lastFileCount : Int = sourceFileCount 
      def sourceFileNames : String = allSourceFiles.map(_.getPath).sortWith(_<_).mkString(" ")
      var lastSourceFileNames : String = sourceFileNames

      def printWaitingMessage = println("\nWaiting for source file changes (press 'enter' to interrupt)")
      def rerunTask{
        println(bld().execute)
        lastTaskTime = Some(System.currentTimeMillis)
        lastFileCount = sourceFileCount
        lastSourceFileNames = sourceFileNames
        printWaitingMessage
      }


      def lastSrcModificationTime = {
        allUpstreamProjects.map(proj => {
            val baseWatchedFiles = proj.compilePhase.sourceFiles ++ proj.testCompilePhase.sourceFiles
            val watchedFiles = baseWatchedFiles ++ (
            if (props.UpdateOnCompile()) {
              log.info("also watching project ivy.xml files")
              proj.ivyFile :: Nil
            } else Nil)
            FileUtils.lastModifiedFileTime(watchedFiles)
          }).max
      }
      printWaitingMessage
      while (true) {
        Thread.sleep(1000)
        if (System.in.available > 0 && System.in.read == 10) return
        (lastTaskTime,  lastSrcModificationTime, lastFileCount, sourceFileCount, lastSourceFileNames, sourceFileNames) match {
          case (None, _, _, _, _, _) => { rerunTask }                        // Task has never been run
          case (Some(t1), Some(t2), _, _, _, _) if t1 < t2 => { rerunTask }  // Code has changed since task last run
          case (_, _, m, n, _, _) if m != n ⇒ { rerunTask }                  // Source file has been added or deleted
          case (_, _, _, _, a, b) if a != b ⇒ { rerunTask }                  // Source file has been renamed
          case _ =>                                                    // Either no code yet or code has not changed
        }
      }
    }
}


