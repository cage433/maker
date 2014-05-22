How hard could it be to replace SBT?
====================================


Purpose
-------

Maker is a scala build tool, written out of frustration with the complexity of SBT. It performs similar tasks, namely compile, test, publish etc - its main aim is to speed up the code/compile/test cycle.


Running Maker
-------------

./<maker-dir>/bin/maker.sh -p <project-file>


Maker will download the required jars, compile itself, and then load the project file.


You should see the list of projects loaded into the repl. Each project can run the usual set of Tasks, these are currently compile, testCompile, clean, test, update, pack. These will run across the project and its dependencies. most of these tasks have a 'just this project' equivalent. E.g. compileOnly, testOnly.

Before compilation the update task needs to be run. For speed this is not an automatic dependency. Ivy dependencies for each project are held in <project>/maker-ivy.xml. Downloaded jars will be placed into <project>/maker-lib.

To run a task continuously, use the following

scala> launcher.~(launcher.compile _)

This will wait for any code changes and then repeat the task. Break out by hitting enter.



 








