println("\n ** Loading Titan Model build...\n")


lazy val makerProps : Props = file("Maker.conf")
lazy val starlingProperties : Properties = file("props.conf")

/**
 * Start of Titan related build and deploy definitions (should probably go in a separate file or even a precompiled lib for speed)
 */
def helpTitan() = {
  println("HelpTitan:")
  println("")
  println("\t titanBinDeps = project defining all binary dependencies of titan")
  println("\t titanComponents - list of titan components built for this env")
  println("\t titanBinDepComponents - list of binary dependencies for deployment")
  println("\t * titanLauncher - starling and titan launcher project")
  println("")
  println("\t buildWithTitan - build all of starling, build titan from sources and only package titan")
  println("")
  println("\t * buildAndDeployWithTitanJetty - build starling + titan from source and copy built wars to jetty")
  println("\t deployTitanJbossWars - deploy titan binary wars to jboss")
  println("")
  println("\t * runStarlingWithTitan - launch starling + titan in the repl")
  println("\t runStarlingWithTitanDeploy(false) - launch staring + titan in the repl with no jetty redeployment")
  println("")
}

/**
 * define hub tooling model projects and build model sources and compile as maker modules
 */
val SCALA_BINDINGS_DIR = "scala-bindings"
def buildSource(root : File, modelFile : File, outputDir : File) = {
  lazy val buildUsingBinaryTooling = true
  val scalaBindingsDir = file(root, SCALA_BINDINGS_DIR)
  def latestRubyFileTime(path : File) = {
    val files = findFilesWithExtension("rb", path)
    if (files.isEmpty)
      throw new Exception("No ruby files found")
    files.map(_.lastModified).toList.sort(_>_).head
  }
  def earliestScalaFileTime(path : File) = {
    findFilesWithExtension("scala", path).toList.map(_.lastModified).sort(_<_) match {
      case Nil => None
      case t :: _ => Some(t)
    }
  }
  val toolingLauncher = if (buildUsingBinaryTooling == true) 
    new File(root, "../../bindinggen.rb")
  else 
    new File(root, "/model/tooling/binding-generator/thubc.rb")

  def generateModelMainSourceCmd() = Command(
      "ruby",
      toolingLauncher.getAbsolutePath,
      "-o", outputDir.getAbsolutePath,
      "-b", file(scalaBindingsDir, "scala-bindings.rb").getAbsolutePath, file(root, "model.rb").getAbsolutePath).exec()
/*
  lazy val nonModelSourcePath = new File(root, "src")
  def copyNonModelSource = {
    if (! (nonModelSourcePath.exists)) {
      import IO._
      val originalSourcePath = new File(titanModuleRoot, "/scala-model-with-persistence/src/")
        copyDirectory(originalSourcePath, nonModelSourcePath)
      val hibernateBean = new File (titanModuleRoot, "/src/main/scala/com/trafigura/refinedmetals/persistence/CustomAnnotationSessionFactoryBean.scala")
        println("***** DEBUG ***** path " + hibernateBean.getAbsolutePath + ", " + hibernateBean.exists + ", " + hibernateBean.canWrite)
      if (hibernateBean.exists && hibernateBean.canWrite) hibernateBean.delete()
    }
    None
  }
*/

  (latestRubyFileTime(root), earliestScalaFileTime(outputDir)) match {
    case (t_ruby, Some(t_scala)) if t_ruby < t_scala =>
      println("Generated code is up to date, nothing to do")
    case _ =>
      generateModelMainSourceCmd()
  }
}

import maker.utils.GroupAndArtifact

/**
 * this was going to wrap a project to provide building of sources, but alas this is tightly coupled to maven so 
 *    for now we just use pre-generated source (from maven or whatever means) and compile as normal.
   TBD whether to make this generate the source or possibly the model gen will be obsolete by then anyway...
 */
case class ModelProject(name : String,
                        root : File,
                        modelFile : File,
                        outputDir : File,
                        ga : Option[GroupAndArtifact] = None,
                        depends : List[Project] = Nil) {
  val scalaBindingsDir = file(root, SCALA_BINDINGS_DIR)
  val project = Project(
      name,
      root,
      sourceDirs = List(outputDir),
      ivyFileRel = SCALA_BINDINGS_DIR + "/maker-ivy.xml",
      moduleIdentity = ga).dependsOn(depends : _*)

  def genModel = buildSource(root, modelFile, outputDir)
  def compile = {
    /*
    genModel match {
      case (0, _) => project.compile
      case _ => println("failed to generate model, aborting")
    }
    */
    project.compile
  }
  def cleanModel = recursiveDelete(outputDir)
  def cleanAll = cleanModel; clean

  def clean = project.clean
  def pack = project.pack
}

val modelRoot = file("../../mdl")
val outDir = "scala-bindings/target/generated-source" // "src"
def mkModelProjectEx(name : String, subdir : String = "public", ga : Option[GroupAndArtifact] = None, depends : List[Project] = Nil) : ModelProject = {
    val root = file(file(modelRoot, name), subdir)
    ModelProject(name,
                 root,
                 file(root, "model.rb"),
                 file(root, outDir), // "src"),
                 ga,
                 depends)          
}
def mkModelProject(name : String, ga : Option[GroupAndArtifact] = None, depends : List[Project] = Nil) : ModelProject = 
      mkModelProjectEx(name, "public", ga, depends)


/**
 * Titan model / bin-dep lib builds
 */
import maker.utils.GroupId._
lazy val trademgmtInternalModel = mkModelProjectEx("trademgmt", "internal", Some("com.trafigura.titan" % "model-trademgmt-internal-scala-bindings"))
lazy val trademgmtPublicModel = mkModelProject("trademgmt", Some("com.trafigura.titan" % "model-trademgmt-public-scala-bindings"), List(trademgmtInternalModel.project))
lazy val logisticsPublicModel = mkModelProject("logistics", Some("com.trafigura.titan" % "model-logistics-public-scala-bindings"))
lazy val trademgmtModelDeps : List[Project] = List(trademgmtPublicModel).map(_.project)
lazy val logisticsModelDeps : List[Project] = List(logisticsPublicModel).map(_.project)

