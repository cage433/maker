# this is the model for Reference Data Project
in_namespace('inner.ReferenceData.Holidays') {
=begin

=end


define('RdHolidayCalendar', :extends => 'RdEntity') {

  field 'shortCode',      :string
  field 'name',           :string
  field 'description',    :string
  field 'calendarType',   :string
  field 'lastUpdate',     :datetime
}


  define('RdHoliday', :extends => 'RdEntity') {
    mixin('MainVersionable')

    field 'name', :string
    field 'date', :datetime

    field 'statusId', :string
    field 'calendarId', :string
  }

}
