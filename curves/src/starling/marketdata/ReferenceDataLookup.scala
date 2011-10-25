package starling.marketdata

import starling.market.{Commodity, FuturesMarket, NeptuneCommodity, NeptunePricingExchange}


trait ReferenceDataLookup {
  def name : String
  def areaCodeFor(code: NeptuneCountryCode): Option[AreaCode]
  def areaFor(code: AreaCode): Area
  def areaFor(code: NeptuneCountryCode): Option[Area] 
  def gradeFor(code: GradeCode): Grade
  def contractLocationFor(code: ContractualLocationCode): ContractualLocation
  def countryFor(code: NeptuneCountryCode): NeptuneCountry
  def incotermFor(code: IncotermCode): Incoterm
  def marketFor(commodity : NeptuneCommodity, countryCode : NeptuneCountryCode) : FuturesMarket = {
    val area = areaFor(countryCode).getOrElse{
      throw new Exception("Can't find area for country code *" + countryCode
       + "*, in this " + this.getClass)
    }
    marketFor(commodity, area.code)
  }
  def marketFor(commodity : NeptuneCommodity, areaCode : AreaCode) : FuturesMarket = {
    val exchange = NeptunePricingExchange.fromArea(areaCode).getOrElse{
      throw new Exception("Can't find exchange for area code " + areaCode)
    }
    exchange.marketFor(commodity).getOrElse{
      throw new Exception("Don't know what futures market to use for " + commodity + ", exchange " + exchange)
    }
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
  val Null = new NullReferenceDataLookup()
  class NullReferenceDataLookup extends ReferenceDataLookup{
    def name = "Null Reference Data Lookup"
    def areaCodeFor(code: NeptuneCountryCode) = None
    def areaFor(code: AreaCode) = Area(code, unknownName)
    def areaFor(code: NeptuneCountryCode) : Option[Area] = None
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
