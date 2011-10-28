package starling.marketdata

import starling.market.{FuturesExchange, FuturesMarket, NeptuneCommodity}
trait ReferenceDataLookup {
  def name : String
  final def areaCodeFor(code: NeptuneCountryCode): Option[AreaCode] = countries(code).area.map(_.code)
  final def areaFor(code: AreaCode): Area = areas(code)
  final def areaFor(code: NeptuneCountryCode): Option[Area] = countries(code).area
  final def gradeFor(code: GradeCode): Grade = grades(code)
  final def contractLocationFor(code: ContractualLocationCode): ContractualLocation = contractLocations(code)
  final def countryFor(code: NeptuneCountryCode): NeptuneCountry = countries(code)
  final def incotermFor(code: IncotermCode): Incoterm = incoterms(code)
  def marketFor(commodity : NeptuneCommodity, countryCode : NeptuneCountryCode) : FuturesMarket = {
    val area = areaFor(countryCode).getOrElse{
      throw new Exception("Can't find area for country code *" + countryCode
       + "*, in this " + this.getClass)
    }
    marketFor(commodity, area.code)
  }
  def marketFor(commodity : NeptuneCommodity, areaCode : AreaCode) : FuturesMarket = {
    val exchange = FuturesExchange.fromArea(areaCode).getOrElse{
      throw new Exception("Can't find exchange for area code " + areaCode)
    }
    exchange.inferMarketFromCommodity(commodity).getOrElse{
      throw new Exception("Don't know what futures market to use for " + commodity + ", exchange " + exchange)
    }
  }

  val areas: Map[AreaCode, Area]
  val contractLocations: Map[ContractualLocationCode, ContractualLocation]
  val countries: Map[NeptuneCountryCode, NeptuneCountry]
  val grades: Map[GradeCode, Grade]
  val incoterms: Map[IncotermCode, Incoterm]
}

case class Incoterm(code: IncotermCode, name: String) {
  def tuple = (code.code, name)
  override def toString = name
}

object ReferenceDataLookup {
  val Null = new NullReferenceDataLookup()
  class NullReferenceDataLookup extends ReferenceDataLookup {
    def name = "Null Reference Data Lookup"
    val areas = unknown(Area.apply)
    val contractLocations = unknown(ContractualLocation.apply)
    val incoterms = unknown(Incoterm.apply)
    val grades = unknown(Grade.apply)
    val countries = Map.empty[NeptuneCountryCode, NeptuneCountry].withDefault(NeptuneCountry(_, unknownName, None))

    private def unknown[K, V](default: (K, String) => V) = Map.empty[K, V].withDefault(k => default(k, unknownName))
    private val unknownName = "ReferenceDataLookup.Null"
  }
}
