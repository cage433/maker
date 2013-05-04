package maker.project

import maker.utils.FileUtils._
import java.io.File
import maker.MakerProps
import maker.task.Dependency
import maker.task.compile.CompileTask

class TopLevelProject(
  override val name : String,
  upstreamProjects:List[Project],
  props : MakerProps = MakerProps(
    "MakerLogLevel", "ERROR", 
    "ShowCompilerOutput", "false", 
    "ShowTestProgress", "false", 
    "StripInfoFromTaskResults", "false"
  ),
  topLevelExcludedFolders:List[String] = Nil
) extends Project(
  root = file("."),
  name = name,
  layout = new MavenProjectLayout(file(".")){
    override def unmanagedLibDirs = Set[File]()
  },
  upstreamProjects = upstreamProjects,
  // Force TestCompile as top level to compile _all_
  upstreamTestProjects = upstreamProjects.flatMap(_.allUpstreamProjects).distinct,
  props = props
) {
  private val generator = IDEAProjectGenerator(props)
  def generateIDEAProject() {
    val allModules = allUpstreamProjects.flatMap(_.allUpstreamProjects).distinct

    generator.generateTopLevelModule(rootAbsoluteFile, name, topLevelExcludedFolders)
    generator.generateIDEAProjectDir(rootAbsoluteFile, name)
    allModules.foreach(module => generator.generateModule(module))
    generator.generateModulesFile(file(rootAbsoluteFile, ".idea"), allModules.map(_.name))
  }
  def downloadScalaCompilerAndLibrary{
    import maker.utils.RichString._
    val ivyText="""
<ivy-module version="1.0" xmlns:e="http://ant.apache.org/ivy/extra">
  <info organisation="maker" module="maker"/>
  <configurations>
    <conf name="default" transitive="false"/>
  </configurations>
  <dependencies defaultconfmapping="*->default,sources">
    <dependency org="org.scala-lang" name="scala-compiler" rev="%s"/>
    <dependency org="org.scala-lang" name="scala-library" rev="%s"/>
  </dependencies>
</ivy-module>
    """ % (props.ScalaVersion(), props.ScalaVersion())
    val ivyFile = file(".maker/scala-library-ivy.xml")
    writeToFile(
      ivyFile,
      ivyText
    )
  }

}
