println("\n ** Loading Starling build (script)  ...\n")

import java.io.File; import org.apache.commons.io.FileUtils._
import maker.project._;
import maker.utils.FileUtils._;
import maker.task.BuildResult;
//import maker.Maker._;
import maker.MakerProps._
import Common._;
import Utils._;
import Starling._;
import Starling.starling._;
System.setProperty("scala.usejavacp", "false")

println("\nStarling build loaded\n\nNote: for convenience the 'starling' project is in the root scope, clean, test etc will act on that unless a project is specified (e.g. utils.clean...)\n")

