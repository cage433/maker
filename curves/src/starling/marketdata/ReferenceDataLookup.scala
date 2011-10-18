package starling.marketdata

import starling.market.{Commodity, FuturesMarket, NeptuneCommodity, NeptunePricingExchange}


trait ReferenceDataLookup {
  def areaCodeFor(code: NeptuneCountryCode): Option[AreaCode]
  def areaFor(code: AreaCode): Area
  def areaFor(code: NeptuneCountryCode): Option[Area] 
  def gradeFor(code: GradeCode): Grade
  def contractLocationFor(code: ContractualLocationCode): ContractualLocation
  def countryFor(code: NeptuneCountryCode): NeptuneCountry
  def incotermFor(code: IncotermCode): Incoterm
  def marketFor(commodity : NeptuneCommodity, countryCode : NeptuneCountryCode) : FuturesMarket = {
    areaFor(countryCode).map{area => marketFor(commodity, area.code)}.getOrElse(Commodity.standardFuturesMarket(commodity))
  }
  def marketFor(commodity : NeptuneCommodity, areaCode : AreaCode) : FuturesMarket = {
    NeptunePricingExchange.fromArea(areaCode).flatMap(_.marketFor(commodity)).getOrElse(Commodity.standardFuturesMarket(commodity))
  }

  def areaCodes: Set[AreaCode]
  def contractLocationCodes: Set[ContractualLocationCode]
  def countryCodes: Set[NeptuneCountryCode]
  def gradeCodes: Set[GradeCode]
  def incotermCodes: Set[IncotermCode]
}

case class Incoterm(code: IncotermCode, name: String) {
  override def toString = name
}

object ReferenceDataLookup {
  object Null extends ReferenceDataLookup {
    def areaCodeFor(code: NeptuneCountryCode) = None
    def areaFor(code: AreaCode) = Area(code, unknownName)
    def areaFor(code: NeptuneCountryCode) = None
    def gradeFor(code: GradeCode) = Grade(code, unknownName)
    def contractLocationFor(code: ContractualLocationCode) = ContractualLocation(code, unknownName)
    def countryFor(code: NeptuneCountryCode) = NeptuneCountry(code, unknownName, None)
    def incotermFor(code: IncotermCode) = Incoterm(code, unknownName)
    def contractLocationCodes = Set()
    def incotermCodes = Set()
    def countryCodes = Set()
    def gradeCodes = Set()
    def areaCodes = Set()

    private val unknownName = "ReferenceDataLookup.Null"
  }
}
