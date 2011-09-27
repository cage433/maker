package starling.curves.readers

import starling.db.{MarketDataEntry, MarketDataSource}
import starling.dbx.QueryBuilder._
import starling.daterange.{ObservationPoint, Day}
import starling.richdb.RichDB
import scalaz.Scalaz._
import collection.immutable.Map
import starling.marketdata._


class NeptuneFreightParityMarketDataSource(neptuneDB: RichDB) extends MarketDataSource {
  def read(day: Day) = Map((day, day, FreightParityDataType) → readFreightParity(day))

  def readFreightParity(observationDay: Day): List[MarketDataEntry] = neptuneDB.queryWithResult(
    select("cont_term_code, cont_loc_code, del_term_code, del_area_code, parity_rate, narrative")
      from("[live].[parity_rates]")
  ) { rs =>

    MarketDataEntry(ObservationPoint(observationDay),
      FreightParityDataKey(IncotermCode(rs.getString("cont_term_code")), ContractualLocationCode(rs.getString("cont_loc_code")),
        IncotermCode(rs.getString("del_term_code")), NeptuneCountryCode(rs.getString("del_area_code"))),
      FreightParityData(rs.getDouble("parity_rate"), rs.getString("narrative") ?? ""))
  }
}