package starling.manual

import starling.services.StarlingInit
import starling.props.PropsHelper
import starling.curves.readers.NeptuneBenchmarksMarketDataSource
import starling.daterange.Day

/**
 * Created by IntelliJ IDEA.
 * User: thomas
 * Date: 24/06/11
 * Time: 11:47
 * To change this template use File | Settings | File Templates.
 */

object NeptuneBenchmarkRunner {

  def main(args:Array[String]) {
    val init = new StarlingInit(PropsHelper.defaultProps, false, false, false, false)

    val source = new NeptuneBenchmarksMarketDataSource(init.neptuneRichDB)
    source.read(Day.today).foreach(println)

    println("DONE")
  }

}