How hard is it to replace SBT?
==============================

Maker is a Scala build tool. It was developed as an in-house replacement for SBT, with an emphasis on developer 
workflow. 


Outline
-------

This section is intended to clarify the terminology used within Maker - there should be few surprises.

A **module** is a collection of source files corresponding to a compilation unit. A **project** is a collection of
modules with explicit source/test dependencies.

A **task** is an action that can be performed on a module - maker supports the usual tasks one would expect, 
such as compile, testCompile, publish etc. Tasks have dependencies which (unless explicitly instructed) must always run successfully
first, More details on each task are given below. Creating new tasks is straightforward.

A **build** is a collection of tasks, typically the transitive closure of one (or occasionally several) tasks.


Tasks
-----

Suppose we have a project that consists of three modules; A, B and C. B has source dependency on A, C has both a source and
test dependency on A. Note that by a test dependency we mean that the test classes in C have a compile time dependency on test 
classes in A. In that case the dependencies for typical tasks are

| Module | Task    | Upstream Dependencies    |
| ------ | ------- | ------------------------ |
| C      | compile | A compile, A testCompile |

