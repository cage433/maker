# EDM package EDM.Enterprise.Common Domains.Reference Data.Master Data.Units Of Measure
in_namespace('commondomains.referencedata.masterdata.unitsofmeasure') {

=begin
  Unit of Measure - object that represents any units which
  can be used to measure something.
=end
  define('UnitOfMeasure', :extends => 'EnumeratedItem') {
    # Name of the Unit Of Measure.
    field 'name',         :string
  }

=begin
  This enumerates units of measurement that correspond to Fundamental Unit Type.
  For example, the Fundamental Unit Type of Weight might have
  fundamental units of measure corresponding to kilograms or tons.
=end
  define('FundamentalUnitOfMeasure', :extends => 'UnitOfMeasure') {
    # The type of fundamental unit.
    field 'unitType',       'FundamentalUnitType'
  }

=begin
  A fundamental unit of measure
  representing a particular financial currency.
=end
  define('Currency', :extends => 'FundamentalUnitOfMeasure') {
    # The number of decimal places
    #that represent the minor currency unit.
    # For example, for US dollars it is 2,
    # since there are 100 (10^2) cents in a dollar.
    field 'minorUnit',                    :string
    # Indicates whether or not invoices
    # in this currency are eligible for securitisation.
    field 'eligibleForSecurisation',      :boolean
    field 'synthetic',                    :boolean
    # The holidays calendar.
    field 'holidays',                     'commondomains.referencedata.masterdata.calendars.HolidaysCalendar'
  }
}
