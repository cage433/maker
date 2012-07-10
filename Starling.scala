println("\n ** Loading Starling build (script)  ...\n")

import java.io.File
import org.apache.log4j.Level._
import org.apache.commons.io.FileUtils._
import maker.project._
import maker.utils.FileUtils._
import maker.utils.Log
import maker.utils.Log._
import maker.task.BuildResult
import maker.Maker._

//:load maker/project/common.scala
import Common._

//:load maker/project/utils.scala
import Utils._

//:load maker/project/titan-model.scala
import TitanModel._

//:load maker/project/starling-modules.scala
import Starling._

import Starling.starling._

println("\nStarling build loaded\n\nNote: for convenience the 'starling' project is in the root scope, clean, test etc will act on that unless a project is specified (e.g. utils.clean...)\n")

