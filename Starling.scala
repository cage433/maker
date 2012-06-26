println("\n ** Loading Starling build..\n")

import java.util.Properties
import java.io.File
import org.apache.log4j.Level._
import org.apache.commons.io.FileUtils._
import maker.project._
import maker.project.TopLevelProject
import maker.project.ProjectLib
import maker.Props
import maker.utils.FileUtils._
import maker.utils.Log
import maker.utils.Log._
import maker.RichProperties._
import maker.utils.os.Command
import maker.utils.os.Command._
import maker.utils.ModuleId._
import maker.utils.GroupAndArtifact
import maker.task.BuildResult

:load maker/common.scala
:load maker/utils.scala
:load maker/titan-model.scala
:load maker/starling-modules.scala

