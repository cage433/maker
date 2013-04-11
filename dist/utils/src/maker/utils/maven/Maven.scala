package maker.utils.maven

import maker.utils.DependencyLib

case class ScmDef(url : String, connection : String)
case class MavenRepository(id : String, name : String, url : String, layout : String)

// project peer module related dependencies
case class ProjectDef(description : String,
                      moduleLibDef : DependencyLib,             // this project module's identity as a library
                      dependencyModules : List[DependencyLib]   // the peer modules this project module depends on
                       ) {
  def withDeps(deps : List[DependencyLib]) = this.copy(dependencyModules = deps)
}

// top level module definition
case class ModuleDef(projectDef : ProjectDef,
                     dependencies : List[DependencyLib],        // actual 3'd party dependency libs (of this module)
                     repositories : List[MavenRepository],      // and the repos they come from
                     scmDef : ScmDef,                           // SCM details
                     licenses : String,
                     developers : String) {
  def withProjectDef(pd : ProjectDef) = this.copy(projectDef = pd)
  def withDeps(deps : List[DependencyLib]) = this.copy(dependencies = deps)
}
