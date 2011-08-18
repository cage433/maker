# EDM package EDM.Enterprise.Common Domains.Reference Data.Master Data.Units Of Measure
in_namespace('commondomains.referencedata.masterdata.unitsofmeasure') {
=begin
  This service provides methods for retrieving Currencies entities
=end
  service('Units Of Measure Service') {
=begin
    The operation returns the list of Currenciese entites defined in the
    SRD databases. If no record found and empty list should be returned.
=end
    operation('Get Currencies', :returns => list('commondomains.referencedata.masterdata.unitsofmeasure.Currency')) {
      # Base method params
      parameter 'baseParams', 'BaseServiceParams'
    }

=begin
    Returns a Currency as per the guid available in SRD according the parameters passed
    If no Currency available the method returns null
=end
    operation('Get Currency By GUID', :returns => 'commondomains.referencedata.masterdata.unitsofmeasure.Currency') {
      # The GUID for the Currency
      parameter 'guid',               :guid
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

=begin
    Returns a Currency as per the short code available in SRD according the parameters passed
    If no Currency available the method returns null
=end
    operation('Get Currency By Short Code', :returns => 'commondomains.referencedata.masterdata.unitsofmeasure.Currency') {
      # The short code for the Currency
      parameter 'shortCode',          :string
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }
  }
}
