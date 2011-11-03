package starling.reports.impl.pivot.tools

import io.Source
import java.io.{PrintWriter, File}
import xml.pull.{EvElemEnd, EvElemStart, XMLEventReader}

/**
 * Parses Aspect's massive (~500MB) xml files that contain the EOY mtm per trade.
 * Creates a txt file we can use in Starling
 */
object ParseAspectEOYFiles {

  def main(args: Array[String]) {
    val src = Source.fromFile("/home/david.corcoran/tmp/NaphthaSpec_Yearly_1011.xml")
    val outFile = new File("/home/david.corcoran/tmp/NaphthaSpec_Yearly_1011.txt")
    val out = new PrintWriter(outFile)
    val er = new XMLEventReader(src)
    var tradeID: Option[String] = None
    var totalAct = ""
    var totalMtm = ""
    var cost = false
    val ids = scala.collection.mutable.Set[String]()
    while (er.hasNext) {
      er.next match {
        case e@EvElemStart(_, "pnl", _, _) => {
          assert(tradeID.isEmpty)
          val b = e.attrs.asAttrMap
          tradeID = e.attrs.asAttrMap.get("externalid").map(_.toString)
        }
        case e@EvElemStart(_, _, _, _) if e.attrs.asAttrMap.get("name") == Some("total-cost") => {
          assert(tradeID.isDefined)
          cost = true
        }
        case e@EvElemStart(_, _, _, _) if e.attrs.asAttrMap.get("name") == Some("total-act") => {
          assert(tradeID.isDefined)
          totalAct = e.attrs.asAttrMap("value").toString
        }
        case e@EvElemStart(_, _, _, _) if e.attrs.asAttrMap.get("name") == Some("total-mtm") => {
          assert(tradeID.isDefined)
          totalMtm = e.attrs.asAttrMap("value").toString
        }
        case e@EvElemEnd(_, "pnl") => {
          assert(tradeID.isDefined)
          val id = tradeID.get
          if (cost || ids.contains(id)) {
            // skip
          } else {
            assert(totalMtm.nonEmpty, id + ", " + e)
            assert(totalAct.nonEmpty, id + ", " + e)
            ids += id
            out.println(id + "\t" + totalAct + "\t" + totalMtm)
          }
          tradeID = None
          totalAct = ""
          totalMtm = ""
          cost = false
        }
        case bla => {
          val a = bla
          //          println("bla + " + a)
        }
      }
    }
    println("flushing")
    out.flush()
    out.close()
    println("Done here")
  }
}