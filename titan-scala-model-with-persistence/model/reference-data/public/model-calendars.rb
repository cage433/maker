# EDM package EDM.Enterprise.Common Domains.Reference Data.Master Data.Calendars
in_namespace('commondomains.referencedata.masterdata.calendars') {
=begin
  Entry in a calendar.
=end
  define('CalendarEntry', :extends => 'Entity') {
    field 'entryDate',      :datetime
    field 'name',           :string
    field 'status',          'Status'
  }
=begin
  A set of dates.
=end
  define('Calendar', :extends => 'EnumeratedItem') {
    field 'name',           :string
    field 'lastUpdate',     :datetime
    field 'calendarType',   :string
    field 'entries',        :list, :element_type => 'CalendarEntry'
  }

=begin
  A set of dates that are public holidays.
=end
  define('HolidaysCalendar', :extends => 'Calendar') {
  }
}