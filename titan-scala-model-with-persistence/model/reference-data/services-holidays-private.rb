in_namespace('inner.ReferenceData.Holidays') {

  service('Holiday Service') {

    # Returns Holidays for Holiday Calendar
    operation('Get Holidays', :returns => list('RdHoliday')) {
      parameter 'calendarGUID', :string
    }

    # Returns all Holiday Calendars
    operation('Get Holiday Calendar List', :returns => list('RdHolidayCalendar')) {
    }

  }

}