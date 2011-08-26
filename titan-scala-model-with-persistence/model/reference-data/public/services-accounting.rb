# EDM package EDM.Enterprise.Common Domains.Reference Data.Master Data.Accounting
in_namespace('commondomains.referencedata.masterdata.accounting') {
=begin
  This service provides methods for retrieving entities defined in the
  Accounting package along with methods which are returning the
  relationship between the entities defined
=end
  service('Accounting Service') {
=begin
    The operation returns the list of Book Type entites defined in the
    SRD databases. If no record found and empty list should be returned.
=end
    operation('Get Book Types', :returns => list('BookType')) {
        # Base method params
      parameter 'baseParams', 'BaseServiceParams'
    }

=begin
    Returns a Book Type as per the guid available in SRD according the parameters passed
    If no Book Type available the method returns null
=end
    operation('Get Book Type By GUID', :returns => 'BookType') {
        # The GUID for the Book Type
      parameter 'guid', :guid
      # Base method params
      parameter 'baseParams', 'BaseServiceParams'
    }

=begin
    Returns a Book Type as per the short code available in SRD according the parameters passed
    If no Book Type available the method returns null
=end
    operation('Get Book Type By Short Code', :returns => 'BookType') {
        # The short code for the Book Type
      parameter 'shortCode', :string
      # Base method params
      parameter 'baseParams', 'BaseServiceParams'
    }

    #=================================================================================

=begin
    The operation returns the list of Book Group entites defined in the
    SRD databases. If no record found and empty list should be returned.
=end
    operation('Get Book Groups', :returns => list('BookGroup')) {
        # Base method params
      parameter 'baseParams', 'BaseServiceParams'
    }

=begin
    Returns a Book Group as per the guid available in SRD according the parameters passed
    If no Book Group available the method returns null
=end
    operation('Get Book Group By GUID', :returns => 'BookGroup') {
        # The GUID for the Book Group
      parameter 'guid', :guid
      # Base method params
      parameter 'baseParams', 'BaseServiceParams'
    }

=begin
    Returns a Book Group as per the short code available in SRD according the parameters passed
    If no Book Group available the method returns null
=end
    operation('Get Book Group By Short Code', :returns => 'BookGroup') {
        # The short code for the Book Group
      parameter 'shortCode', :string
      # Base method params
      parameter 'baseParams', 'BaseServiceParams'
    }

    #=================================================================================

=begin
    The operation returns the list of Book entites defined in the
    SRD databases. If no record found and empty list should be returned.
=end
    operation('Get Books ',         :returns => list('Book')) {
        # Base method params
      parameter 'baseParams', 'BaseServiceParams'
    }

=begin
    Returns a Book as per the guid available in SRD according the parameters passed
    If no Book available the method returns null
=end
    operation('Get Book By GUID', :returns => 'Book') {
        # The GUID for the Book
      parameter 'guid', :guid
      # Base method params
      parameter 'baseParams', 'BaseServiceParams'
    }

=begin
    Returns a Book as per the short code available in SRD according the parameters passed
    If no Book available the method returns null
=end
    operation('Get Book By Short Code', :returns => 'Book') {
        # The short code for the Book
      parameter 'shortCode', :string
      # Base method params
      parameter 'baseParams', 'BaseServiceParams'
    }

    #=================================================================================
  }
}
