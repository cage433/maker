println("\n ** Loading Starling build..\n")

import java.util.Properties
import java.io.File
import org.apache.log4j.Level._
import org.apache.commons.io.FileUtils._
import maker.project._
import maker.project.TopLevelProject
import maker.project.ProjectLib
import maker.Props
import maker.Props._
import maker.utils.FileUtils._
import maker.utils.Log
import maker.utils.Log._
import maker.utils.os.Command
import maker.utils.os.Command._
import maker.utils.ModuleId._
import maker.utils.GroupAndArtifact
import maker.task.BuildResult

:load maker/common.scala
:load maker/utils.scala
:load maker/titan-model.scala
:load maker/starling-modules.scala

import starling._

println("\nStarling build loaded\n\nNote: for convenience the 'starling' project is in the root scope, clean, test etc will act on that unless a project is specified (e.g. utils.clean...)\n")

