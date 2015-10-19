package maker.project

import maker.task.tasks.UpdateTask
import maker.utils.FileUtils._
import java.io.BufferedWriter
import scala.collection.immutable.VectorBuilder
import org.eclipse.aether.artifact.Artifact
import maker.ScalaVersion

trait Bootstrapper{
  self : Module => 

    def updateMakerScript(marker : String, replacement : Seq[String]){
      val builder = new VectorBuilder[String]()
      val lines = file("maker.py").readLines
      val start = lines.indexWhere(_.startsWith(marker))
      val end = lines.indexWhere(_.startsWith(marker), start + 1)
      builder ++= lines.take(start + 1)
      builder ++= replacement
      builder ++= lines.drop(end)

      withFileWriter(file("maker.py")){
        writer : BufferedWriter => 
          val text = builder.result.mkString("\n")
          writer.println(text)
      }
    }

    def writeBoostrapFile(){
      // TODO - exclusions
      val artifacts = new UpdateTask(self).binaryArtifacts.filterNot(_.getArtifactId == "compiler-interface")
      val bldr = new VectorBuilder[String]()
      def makeLine(artifact : Artifact) = {
        val group = artifact.getGroupId.replace('.', '/')
        val id = artifact.getArtifactId
        val version = artifact.getVersion
        s"""\t(MAVEN, "$group", "$id", "$version")"""
      }
      bldr += "MAKER_DEPENDENCIES  = ["
      artifacts.dropRight(1).foreach{
        artifact => 
          bldr += (makeLine(artifact) + ",")
      }
      bldr += makeLine(artifacts.last)
      bldr += "]"
      updateMakerScript("# GENERATED MAKER DEPENDENCIES", bldr.result)
    }
}
