How hard could it be to replace SBT?
====================================


Purpose
-------

Maker is a scala build tool, written out of frustration with the complexity of SBT. It performs similar tasks, namely compile, test, publish etc - its main aim is to speed up the code/compile/test cycle.

Installation
------------
Maker bootstraps itself from a single script (link here). For a more detailed description see [Installation](markdown/Installation.md)


Creating a project
------------------
A project is a collection of modules (or just a single module). Eash module contains source files, test files, has external dependencies, and may also depend on 'upstream' modules. A two module project will need, at a minimum, the following files and directories.

          project/module-a/src/
                  module-a/tests/
                  module-a/external-resources

          project/module-b/src/
                  module-b/tests/
                  module-b/external-resources

          project/external-resource-config
          project/Project.scala

Note that the layout doesn't match maven style - as I don't like it - however it's straightforward to override the default layout to change the location of (e.g.) the source and test directories.

The project file, Project.scala, defines the modules and any dependencies between them. 

Maker contains single and multi-module sample projects in maker.git/examples, you can begin with one of these.


External Resources
------------------

Originally Maker used Ivy to manage its external dependencies, using standard ivy.xml and ivy-settings.xml files to describe the external resources and resolvers. This was dropped for two reasons. 

* Ivy is *slow* - even when all its dependencies are up to date it can take several seconds to decide it has nothing to  do - this went against the purpose of Maker, to speed up the development cycle. 
* I believe transitive dependencies to be the work of the Devil, and so always explicitly listed the external jars 
  needed - doing this seemed to remove much of Ivy's purpose

Instead of Ivy, Maker simply uses curl to download external jars. Each module keeps its dependencies in the file module/external-resources, each line defines a resource, with the syntax `org name version`. A single top level config file, external-resource-config, is used to maintain a list of resolvers, and also abbreviations. See the sample projects included for examples of these files - or even Maker itself.


Launching Maker
---------------

Having created a project file, simply run the script maker.git/bin/maker.sh - this will bootstrap maker, load the project file, and launch the scala repl. 


Maker Tasks
---------------

Having launched the repl Maker can perform the usual tasks one would expect from a build tool.  The syntax is simply `project-or-module.task`. Available tasks include `update`, `compile`, `testCompile`, `test`, `package`, `publish`.

Maker knows the dependencies between tasks, both across tasks of different types (e.g. `compile` must be executed before `testCompile`) and also taking into account module dependencies (all upstream compilation must succeed before any module is compiled).

