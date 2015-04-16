package maker.project

import maker.task.BuildResult
import maker.task.tasks._
import maker.utils.FileUtils._
import java.io.File
import maker.utils.RichString._
import maker.utils.os.Command
import scala.collection.immutable.Nil
import maker.task.compile.SourceCompilePhase
import com.typesafe.config.{ConfigFactory, Config}

case class Project(
  name : String,
  root : File,
  immediateUpstreamModules:List[Module],
  config : Config = ConfigFactory.load(),
  topLevelExcludedFolders:List[String] = Nil
) extends TmuxIntegration{

  def projectRoot = root.asAbsoluteFile
  override def createDeploy(buildTests: Boolean = true, version: Option[String] = None): BuildResult =
    executeWithDependencies(CreateDeployTask(this, buildTests))

  val upstreamModulesForBuild = allUpstreamModules
  override def toString = name

  def constructorCodeAsString : String = {
    val b = new StringBuffer
    allUpstreamModules.foreach{
      m => 
        b.addLine(m.constructorCodeAsString)
    }
    b.addLine("""val %s = Project("%s", file("%s"), %s)""" % (name, name, root.getAbsolutePath.toString, allUpstreamModules.mkString("List(", ", ", ")")))
    b.toString
  }
  def docOutputDir = file(rootAbsoluteFile, "docs")
  def packageDir = file(rootAbsoluteFile, "package")

  def allUpstreamModules = immediateUpstreamModules.flatMap(_.allUpstreamModules).distinct
  def allUpstreamTestModules = (immediateUpstreamModules ++ immediateUpstreamTestModules).distinct.flatMap(_.allUpstreamTestModules).distinct
  def testClassNames() = {
    allUpstreamModules.flatMap(_.testClassNames())
  }
  def immediateUpstreamTestModules : List[Module] = Nil
  private val generator = IDEAProjectGenerator(this)
  def allModules = allUpstreamModules.flatMap(_.allUpstreamModules).distinct
  def generateIDEAProject() {

    generator.generateTopLevelModule(rootAbsoluteFile, name, topLevelExcludedFolders)
    generator.generateIDEAProjectDir(rootAbsoluteFile, name)
    allModules.foreach(module => generator.generateModule(module))
    generator.generateModulesFile(file(rootAbsoluteFile, ".idea"), this)
  }

  def generateEnsimeProject() {
    val generator = new EnsimeGenerator()
    generator.generateModules(rootAbsoluteFile, name, allModules)
  }

  def generateSbtProject() {
    new SbtGenerator().generate(this)
  }

  def graphvizDiagram(): String = {
    def deps(): List[(Module, Module)] = {
      var seen = Set[Module]()
      var deps = List[(Module, Module)]()
      def collect(modules: Iterable[Module]): Unit =
        for (module <- modules) {
          if (!seen.contains(module)) {
            seen += module
            val upstream = module.immediateUpstreamModules
            upstream.foreach { upM => deps ::= (module, upM) }
            collect(upstream)
          }
        }
      collect(immediateUpstreamModules)
      deps
    }

    deps().map {
      case (fromM, toM) => "\t\"" + fromM + "\" -> \"" + toM + "\"\n"
    }.mkString("digraph \"" + name + "\" {\n", "", "}\n")
  }

  def createModulesDiagram(): Unit = {
    withTempFile(dotFile => {
      writeToFile(dotFile, graphvizDiagram())

      import maker.utils.os.Command
      Command("dot", "-Tpdf", "-o" + name + ".pdf", dotFile.getAbsolutePath).exec
    })
  }

  def publish(version : String, resolver : String, signArtifacts : Boolean = false, includeUpstreamModules : Boolean = false) = {
    require(includeUpstreamModules, "Project publication must include all upstream modules")
    val task = PublishTask(this, allUpstreamModules, resolver, version, signArtifacts) 
    executeWithDependencies(task)
  }
}
