package maker.project

import maker.task.tasks.UpdateTask
import maker.utils.FileUtils._
import java.io.BufferedWriter
import scala.collection.immutable.VectorBuilder

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
      val artifacts = new UpdateTask(self, forceSourceUpdate = false).binaryArtifacts
      val bldr = new VectorBuilder[String]()
      bldr += "MAKER_DEPENDENCIES  = ["
      artifacts.foreach{
        artifact => 
          val group = artifact.getGroupId.replace('.', '/')
          val id = artifact.getArtifactId
          val version = artifact.getVersion
          bldr += s"\t(MAVEN, $group, $id, $version)"
      }
      bldr += "]"
      updateMakerScript("# GENERATED MAKER DEPENDENCIES", bldr.result)
    }
}
