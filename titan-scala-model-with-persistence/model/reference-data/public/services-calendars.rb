# EDM package EDM.Enterprise.Common Domains.Reference Data.Master Data.Calendars
in_namespace('commondomains.referencedata.masterdata.calendars') {
=begin
  This service provides methods for retrieving Holiday entities
  along with methods which are returning the
  relationship between the entities defined
=end
  service('Holidays Service') {
=begin
    The operation returns the list of Holidays Calendar entites defined in the
    SRD databases. If no record found and empty list should be returned.
=end
    operation('Get Holidays Calendars', :returns => list('commondomains.referencedata.masterdata.calendars.HolidaysCalendar')) {
        # Base method params
      parameter 'baseParams', 'BaseServiceParams'
    }

=begin
    Returns a Holidays Calendar as per the guid available in SRD according the parameters passed
    If no Holidays Calendar available the method returns null
=end
    operation('Get Holidays Calendars By GUID', :returns => 'commondomains.referencedata.masterdata.calendars.HolidaysCalendar') {
        # The GUID for the Book Type
      parameter 'guid', :guid
      # Base method params
      parameter 'baseParams', 'BaseServiceParams'
    }

=begin
    Returns a Holidays Calendar as per the short code available in SRD according the parameters passed
    If no Holidays Calendar available the method returns null
=end
    operation('Get  Holidays Calendars By Short Code', :returns => 'commondomains.referencedata.masterdata.calendars.HolidaysCalendar') {
        # The short code for the Book Type
      parameter 'shortCode', :string
      # Base method params
      parameter 'baseParams', 'BaseServiceParams'
    }
  }
}
