println("\n ** Loading master build..\n")

import java.util.Properties
import java.io.File
import org.apache.log4j.Level._
import org.apache.commons.io.FileUtils._
import maker.project._
import maker.project.Project
import maker.project.TopLevelProject
import maker.Props
import maker.utils.FileUtils._
import maker.utils.MakerLog
import maker.utils.os.Command
import maker.utils.os.Command._
import maker.utils.ModuleId._
import maker.utils.GroupAndArtifact
import maker.task.BuildResult
import maker.task.TaskFailed
import maker.task.TaskSucceeded

:load maker/common.scala
:load maker/utils.scala
:load maker/titan-model.scala
:load maker/starling-modules.scala
:load maker/titan-modules.scala

//repl.setPrompt("master>")

