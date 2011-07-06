package starling.schemaevolution

import system.Patch
import starling.services.StarlingInit
import starling.richdb.RichDB
import starling.db.DBWriter
import starling.pivot.{PivotFormatter, DecimalPlaces}


class Patch99_AddPercentDecimalPlacesSetting extends Patch {
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    val patcher = new CaseClassPatcher[DecimalPlaces](map => DecimalPlaces(
      map("defaultFormat"), map("lotsFormat"), map("priceFormat"), map("currencyFormat"), PivotFormatter.PercentFormat))

    starling.inTransaction { writer =>
      writer.queryForUpdate("select settings from usersettings where settings is not null") {
        rs => rs.update(Map("settings" -> patcher.patch(rs.getString("settings"))))
      }
    }
  }
}