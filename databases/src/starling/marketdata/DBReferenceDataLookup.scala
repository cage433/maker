package starling.marketdata

import starling.richdb.RichDB


case class DBReferenceDataLookup(neptuneDB: RichDB) extends ReferenceDataLookup {
  private lazy val areas             = neptuneDB.lookupTable("live.geographical_location", "code",         "description")
  private lazy val contractLocations = neptuneDB.lookupTable("live.contract_location",     "code",         "description")
  private lazy val countries         = neptuneDB.lookupTable("live.country",               "code",         "name"       )
  private lazy val countryToArea     = neptuneDB.lookupTable("live.location_area_link",    "country_code", "area_code"  )
  private lazy val deliveryTerms     = neptuneDB.lookupTable("live.delivery_term",         "code",         "description")
  private lazy val grades            = neptuneDB.lookupTable("live.category",              "code",         "description")

  def areaCodeFor(code: NeptuneCountryCode): Option[AreaCode] = areaFor(code).map(_.code)
  def areaFor(code: AreaCode) = Area(code, areas(code.code))
  def gradeFor(code: GradeCode) = Grade(code, grades(code.code))
  def contractLocationFor(code: ContractualLocationCode) = ContractualLocation(code, contractLocations(code.code))
  def countryFor(code: NeptuneCountryCode) = NeptuneCountry(code, countries(code.code), areaFor(code))
  def incotermFor(code: IncotermCode) = Incoterm(code, deliveryTerms(code.code))

  private def areaFor(code: NeptuneCountryCode): Option[Area] = countryToArea.get(code.code).flatMap(areaFor(_))
  private def areaFor(code: String): Option[Area] = areas.get(code).map(Area(AreaCode(code), _))
}