package starling.marketdata

import starling.richdb.RichDB
import starling.utils.ImplicitConversions._


case class DBReferenceDataLookup(neptuneDB: RichDB) extends ReferenceDataLookup {
  private val countryToAreaLT = neptuneDB.lookupTable("live.location_area_link", "country_code", "area_code")
    .mapKeys(NeptuneCountryCode(_)).mapValues(AreaCode(_))

  def name = "Neptune DB Reference Data Lookup"
  val areas = neptuneDB.lookupTable("live.geographical_location", "code", "description", AreaCode.apply, Area.apply)
  val incoterms = neptuneDB.lookupTable("live.delivery_term", "code", "description", IncotermCode.apply, Incoterm.apply)
  val grades = neptuneDB.lookupTable("live.category", "code", "description", GradeCode.apply, Grade.apply)

  val contractLocations = neptuneDB.lookupTable("live.contract_location", "code", "description",
    ContractualLocationCode.apply, ContractualLocation.apply)

  val countries = neptuneDB.lookupTable[NeptuneCountryCode, NeptuneCountry]("live.country", "code", "name",
    NeptuneCountryCode.apply, (code, name) => NeptuneCountry(code, name, countryToAreaLT.get(code).flatMap(areas.get)))

  require((areas.keySet & AreaCode.hardCoded) == AreaCode.hardCoded, "Reference data has changed, hard coded values are invalid")
}
