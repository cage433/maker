package maker.project

import maker.utils.Utils._
import maker.utils.FileUtils._
import maker.graphviz.GraphVizUtils._
import maker.graphviz.GraphVizDiGrapher._

trait ProjectMetaData{
  self : Project ⇒ 

  /// generate documentation and attempt to open index webpage
  def showDoc {
    val docIndex = file(layout.docDir, "index.html")
    if (doc.succeeded && docIndex.exists)
      openHtmlFile(self.props, docIndex)
    else
      props.log.warn("failed to generate docs")
  }

  def showDependencyProjectGraph(depth : Int = 100, showLibDirs : Boolean = false, showLibs : Boolean = false) = {
    def dependentProjects(project : Project, depth : Int) : List[(Project, List[Project])] = {
      (project, project.upstreamProjects) :: project.upstreamProjects.flatMap(dependentProjects(_, depth - 1))
    }
    showGraph(self.props, makeDot(dependentProjects(this, depth), showLibDirs, showLibs))
  }

  def showDependencyLibraryGraph() = {
    val dependentLibs = allUpstreamProjects.toList.map(proj => proj → proj.classpathJarsOnly.toList.map(_.getPath))
    showGraph(self.props, makeDotFromString(dependentLibs))
  }
}
