package starling.services

import starling.pivot._
import scalaz.Scalaz._
import starling.marketdata.ReferenceDataLookup


object NeptuneReferenceData {
  def areas(referenceDataLookup: ReferenceDataLookup) = ReferenceDataLookupTable(
    "Area Code", "Area", referenceDataLookup.areas)

  def contractLocations(referenceDataLookup: ReferenceDataLookup) = ReferenceDataLookupTable(
    "Contract Location Code", "Contract Location", referenceDataLookup.contractLocations)

  def countries(referenceDataLookup: ReferenceDataLookup) = new NeptuneCountryReferenceData(referenceDataLookup)

  def grades(referenceDataLookup: ReferenceDataLookup) = ReferenceDataLookupTable(
    "Grade Code", "Grade", referenceDataLookup.grades)

  def incoterms(referenceDataLookup: ReferenceDataLookup) = ReferenceDataLookupTable(
    "Incoterm Code", "Incoterm", referenceDataLookup.incoterms)
}

class NeptuneCountryReferenceData(referenceDataLookup: ReferenceDataLookup) extends UnfilteredPivotTableDataSource {
  val all@List(countryCode, areaCode, countryFD, area) = fieldDetails("Country Code", "Area Code", "Country", "Area")

  def fieldDetailsGroups = List(FieldDetailsGroup("Fields", all))
  override val initialState = PivotFieldsState(rowFields = fields(countryCode), dataFields = fields(countryFD, areaCode, area))

  def unfilteredData(pfs: PivotFieldsState) = referenceDataLookup.countries.toList.map { case (code, country) => fields(
    countryCode → code.code,
    areaCode → referenceDataLookup.areaCodeFor(code).fold(_.code, ""),
    countryFD → country.name,
    area → referenceDataLookup.areaFor(code).fold(_.name, "")
  ) }
}