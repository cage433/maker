package starling.curves.readers

import starling.dbx.QueryBuilder._
import starling.daterange.{ObservationPoint, Day}
import scalaz.Scalaz._
import collection.immutable.Map
import starling.marketdata._
import starling.props.PropsHelper
import starling.db.{DB, MarketDataEntry, MarketDataSource}
import starling.richdb.{RichResultSetRowFactory, RichDB}

import starling.utils.ImplicitConversions._

class NeptuneFreightParityUtil(neptuneDB: RichDB) {
  def read(day: Day):MultiMap[(Day, Day, MarketDataType), MarketDataEntry] = readAll()
  def readAll(): MultiMap[(Day, Day, MarketDataType), MarketDataEntry] = {
    val today = Day.today
    val data:List[MarketDataEntry] = neptuneDB.queryWithResult(
      select("cont_term_code, cont_loc_code, del_term_code, del_area_code, parity_rate, narrative")
        from("[live].[parity_rates]")
    ) { rs =>

      MarketDataEntry(ObservationPoint(today),
        FreightParityDataKey(IncotermCode(rs.getString("cont_term_code")), ContractualLocationCode(rs.getString("cont_loc_code")),
          IncotermCode(rs.getString("del_term_code")), NeptuneCountryCode(rs.getString("del_area_code"))),
        FreightParityData(rs.getDouble("parity_rate"), rs.getString("narrative") ?? ""))
    }

    Map((today, today, FreightParityDataType) → data)
  }
}

object NeptuneFreightParityUtil {
  def main(args:Array[String]) {
    val db = new RichDB(PropsHelper.defaultProps.NeptuneDatabase(), new RichResultSetRowFactory)
    val entries = new NeptuneFreightParityUtil(db).readAll.values.iterator.next.groupBy(_.key)
    entries.foreach(println)
  }
}