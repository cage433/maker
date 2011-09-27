package starling.marketdata

import starling.richdb.RichDB


case class DBReferenceDataLookup(neptuneDB: RichDB) extends ReferenceDataLookup {
  private lazy val contractLocations = neptuneDB.lookupTable("live.contract_location",  "code",         "description")
  private lazy val countries         = neptuneDB.lookupTable("live.country",            "code",         "name"       )
  private lazy val countryToArea     = neptuneDB.lookupTable("live.location_area_link", "country_code", "area_code"  )
  private lazy val deliveryTerms     = neptuneDB.lookupTable("live.delivery_term",      "code",         "description")
  private lazy val grades            = neptuneDB.lookupTable("live.category",           "code",         "description")

  def areaFor(code: AreaCode) = Area(code, countries(code.code))
  def areaFor(code: NeptuneCountryCode) = ReferenceDataLookup.Null.areaFor(code) // countryToArea(code)
  def gradeFor(code: GradeCode) = Grade(code, grades(code.code))
  def contractLocationFor(code: ContractualLocationCode) = ContractualLocation(code, contractLocations(code.code))
  def countryFor(code: NeptuneCountryCode) = NeptuneCountry(code, countries(code.code), areaFor(code))
  def incotermFor(code: IncotermCode) = Incoterm(code, deliveryTerms(code.code))
}