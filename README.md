How hard is it to replace SBT?
------------------------------

Maker is a Scala build tool. It was developed as an in-house replacement for SBT, with an emphasis on developer 
workflow. 

The core concepts of maker are Projects/Modules, Tasks and Builds

Projects/Modules
----------------

Taking terminolgoy from IntelliJ, a module is a collection of source files corresponding to a compilation unit. A Project
is either a single Module, or else a collection of many Modules. In the latter case there will typically be 
explicit source and/or test dependencies between the modules. Modules 

