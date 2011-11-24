package starling.schemaevolution

import starling.services.StarlingInit
import starling.richdb.RichDB
import starling.db.DBWriter
import system.{PatchUtils, Patch}


class Patch122_AddHO_Brent extends Patch {
  val s = "eaiquoteid::1758	name::NYMEX Heat 1st Month vs ICE Brent 1st Month	lotSize::Some(1000.0)	tenor::Month	bblPerMT::Some(7.45)	formula::Quote(1.0* MKT(29))- Quote(1.0* MKT(28)) 	volatilityID::None	clearPortPrecision::Some(0)	uom::bbl	type::FormulaIndex/FuturesSpreadMarket	ccy::USD	defaultPrecision::Some(4)"

  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    val market = s.trim.split('\t').map {
      e =>
        e.trim.split("::").toList match {
          case k :: v :: Nil => (k -> v)
        }
    }.toMap

    writer.update("delete from markets where eaiquoteid like '%1758%'")
    writer.insert("markets", market)
  }
}