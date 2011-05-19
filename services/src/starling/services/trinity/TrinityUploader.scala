package starling.services.trinity

import collection.immutable.List
import java.io.{PrintWriter, FileWriter}

import starling.daterange._
import starling.gui.api._
import starling.props.Props
import starling.utils._

import starling.utils.ClosureUtil._
import starling.utils.ImplicitConversions._


class TrinityUploader(fclGenerator: FCLGenerator, xrtGenerator: XRTGenerator, props: Props) {
  def uploadCurve(label: CurveLabel)   = upload(fclGenerator.generate(label),          "curve%s.fcl" % label.observationDay)
  def uploadLibor(observationDay: Day) = upload(xrtGenerator.generate(observationDay), "libor%s.xrt" % observationDay)

  private def upload(lines: List[String], fileName: String) {
    Log.debug("Uploading trinity file: %s/%s (lines: %d)" % (props.TrinityUploadDirectory(), "starling-" + fileName, lines.size))

    using(new FileWriter(props.TrinityUploadDirectory() + fileName, true)) { fileWriter =>
      using(new PrintWriter(fileWriter)) { printWriter => printWriter.println(lines.mkString("\n")) }
    }
  }
}