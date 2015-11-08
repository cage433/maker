How hard could it be to replace SBT?
====================================

Purpose
-------

Maker is a scala build tool, written out of frustration with the complexity of SBT. It performs similar tasks, namely compile, test, publish etc - however it isn't a monad transformation tool and has no DSL. Its aim is simply to speed up the code/compile/test cycle. 

Installation
------------
Download and run the script [maker.py](maker.py) in the directory in which your project will live. You will be prompted for the name of your project, and a minimal config file 
will be created at ./maker/Project.scala. 

Configuration
-------------
This specifies for each module its dependencies, 
both external and on other modules. 

Projects and modules
--------------------
A project is a collection of modules, a module being a unit of compilation. A very simple codebase may just consist of a single module, 
note however that only projects can be published - modules cannot. 

A module contains source files, test files and resources, has external dependencies, and may also depend on 'upstream' modules. By default the 
layout used is the maven standard, however you may notice that the maker codebase does not follow this - purely as I consider the standard to be an ugly, verbose java idiom. 
If you care to join me in this struggle then mix in the [ClassicLayout](maker/src/maker/project/ClassicLayout.scala) trait and structure your projects
thus :-

          project/module-a/src/
                  module-a/tests/
                  module-a/resources
                  module-a/test-resources

          project/module-b/src/
                  module-b/tests/
                  module-b/resources
                  module-b/test-resources

          project/Project.scala


The project file, Project.scala, defines the modules and any dependencies between them. 



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

