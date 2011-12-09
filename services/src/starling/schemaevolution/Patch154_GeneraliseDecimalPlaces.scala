package starling.schemaevolution

import system.Patch
import starling.services.StarlingInit
import starling.richdb.RichDB
import starling.db.DBWriter
import collection.immutable.Map


class Patch154_GeneraliseDecimalPlaces extends Patch {
  protected def runPatch(starlingInit: StarlingInit, starling: RichDB, writer: DBWriter) = {
    val patcher = new ConvertingPatcher[DecimalPlacesOld, DecimalPlacesNew]("decimalPlaces")

    starling.inTransaction { writer =>
      writer.queryForUpdate("select settings from usersettings where settings is not null") { rs => {
        val from = rs.getString("settings")
        val to = patcher.patch(from)

        if (to != from) rs.update(Map("settings" → to))
      } }
    }
  }
}

case class DecimalPlacesOld(defaultFormat: String, lotsFormat: String, priceFormat: String, currencyFormat: String,
                            percentageFormat: String, unlimitedOnExplainScreen: Boolean) extends Convertable[DecimalPlacesNew] {

  def convert = new DecimalPlacesNew(Map("default" → defaultFormat, "lots" → lotsFormat, "price" → priceFormat,
    "currency" → currencyFormat, "percentage" → percentageFormat), unlimitedOnExplainScreen)
}

class DecimalPlacesNew(formats: Map[String, String], val unlimitedOnExplainScreen: Boolean) {
  def defaultFormat = formats("default")
  def lotsFormat = formats("lots")
  def priceFormat = formats("price")
  def currencyFormat = formats("currency")
  def percentageFormat = formats("percentage")
}
