/*
 * Copyright (c) 2011-2012, Alex McGuire, Louis Botterill
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met: 
 *
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer. 
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution. 
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package maker.graphviz

import maker.project.Project
import maker.utils.os.Command
import maker.utils.os.OsUtils._
import java.io.File
import maker.utils.Utils
import maker.task.BuildResult
import maker.task.TaskResult, TaskResult._
import maker.task.compile.CompileTask
import maker.MakerProps
import maker.task.Task

object GraphVizDiGrapher {
  val graphName = "Maker-Project-Graph"
  val defaultFont = "fontname=Helvetica fontsize=8"

  def mkGraph(name : String,  graphDef : String) = {
    val g = "digraph \\\"%s\\\" { graph[size=\\\"100.0, 100.0\\\"]; "
    (g + "{ node[ratio=compress size = \\\"100.0, 100.0\\\" nodesep=\\\"0.1\\\" ranksep=\\\"0.1\\\" %s]; %s } ;}")
      .format(name, defaultFont, graphDef)
  }

  def makeDot(graph : List[(Project, List[Project])], showLibDirs : Boolean = false, showLibs : Boolean = false) : String = {
    def mkProjectDep(p1 : Project,  p2 : Project) =
      "{ node[fillcolor=yellow style=filled]; \\\"%s\\\"->\\\"%s\\\" ;}".format(p1.name, p2.name)

    def mkLibDirDep(p : Project,  d : File) =
      "\\\"%s\\\"->{ node[shape=box color=blue style=filled fillcolor=lightskyblue]; \\\"%s\\\" ;}".format(p.name, d.getPath)

    def mkLibFileDep(d : File, df : File) =
      "\\\"%s\\\"->{ node[color=none; shape=plaintext]; \\\"%s\\\" ;}".format(d.getPath, df.getName())

    val g = graph.distinct.flatMap{ case (proj, deps) => deps.flatMap(pd => {
        mkProjectDep(proj, pd) :: { if (showLibDirs)
              proj.classpathJars.toList.flatMap(libDir => mkLibDirDep(proj, libDir) :: { if (showLibs)
                Option(libDir.listFiles()).map(_.toList).getOrElse(Nil).map(x => mkLibFileDep(libDir, x))
              else Nil
            })
          else Nil
        }
      })
    }
    val dot = mkGraph(graphName, g.distinct.mkString(" "))
    dot
  }

  /**
   * makes simple graphviz digraph definition string from a
   *   Task tree (list of (parent -> list of children))
   */
  def makeDotFromTask(result : BuildResult) : String = {
    val graph = result.graph
    val taskResults = result.results
    val resultByProjectTask = taskResults.map{
      tr ⇒ tr.task → tr
    }.toMap
    val allTimes = taskResults.map(_.timeTaken(EXEC_COMPLETE))
    val numberOfTasks = taskResults.size
    val avgTime = allTimes.sum / numberOfTasks
    def mkLabel(pt : Task) = {
      val tr = resultByProjectTask(pt)
      val size = tr.timeTaken(EXEC_COMPLETE).toDouble / avgTime
      val nodeAttrs = if (!tr.succeeded) 
        " style=filled fillcolor=red" 
      else if (tr.task.isInstanceOf[CompileTask])
        " style=filled fillcolor=lightskyblue" 
      else 
        ""
      "{ \\\"<%s> %s (%d) Took %dms\\\" [width=%f height=%f %s] }"
        .format(tr.task, tr.task.project.name, tr.roundNo.getOrElse(-1), tr.timeTaken(EXEC_COMPLETE), size*1.5, size, nodeAttrs)
    }

    def finishingTime(pt : Task) = resultByProjectTask(pt).timeAt(EXEC_COMPLETE)
      //val pts = result.tree.upstreams.toList
    val pts = result.graph.nodes.map{node ⇒ node → result.graph.upstreams(node)}.toList
    val g = pts match {
      case (pt, _) :: Nil => {
        List("%s".format(mkLabel(pt)))
      }
      case _ => pts.flatMap{case (projectTask, upstreamProjectTasks) => {
        import math._
        val criticalPathFinishingTime = (0L /: upstreamProjectTasks.map(finishingTime(_)))(max)

        def mkArrowAttrs(pt : Task) =
          if (finishingTime(pt) >= criticalPathFinishingTime) "[color=red]"
          else "[%s label=\\\"float=%sms\\\"]".format(defaultFont, (criticalPathFinishingTime - finishingTime(pt)) / 1000000)

        upstreamProjectTasks.map(pdt =>
          "%s->%s %s".format(mkLabel(projectTask), mkLabel(pdt), mkArrowAttrs(pdt)))
      }}
    }
    val dot = mkGraph(graphName, g.distinct.mkString(" "))
    println("dot = " + dot)
    dot
  }

  def makeDotFromString(graph : List[(Project, List[String])]) : String = {
    val g = graph.distinct.flatMap(pd => pd._2.map(p =>
          "\\\"-Project-%s\\\"->\\\"%s\\\"".format(pd._1.name, p))).mkString(" ")
    val dot = mkGraph(graphName, g.distinct.mkString(" "))
    println("dot = " + dot)
    dot
  }
}

object GraphVizUtils {
  import Utils._
  val DEFAULT_IMAGE_FORMAT = "png"
  def imageBaseName = "maker-gv-tmp"
  def defaultImageFile = new File(imageBaseName + "." + DEFAULT_IMAGE_FORMAT)

  def removeGraphFile(file : File = defaultImageFile) = {
    file.delete()
    file
  }

  def createGraphFile(props : MakerProps, graphDef : String, file : File = defaultImageFile) = {
    Command(props.log, "/bin/sh", "-c", "echo \"" + graphDef + "\" | dot -T" + DEFAULT_IMAGE_FORMAT + " > " + file.getAbsolutePath).exec()
    file
  }

  def showGraph(props : MakerProps, graphDef : String, file : File = defaultImageFile) =
    showImage(props, createGraphFile(props, graphDef, removeGraphFile(file)))
}
