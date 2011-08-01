# EDM package EDM.Enterprise.Common Domains.Reference Data.Master Data.Locations
in_namespace('commondomains.referencedata.masterdata.locations') {
=begin
  This service provides methods for retrieving entities defined in the
  Locations package along with methods which are returning the
  relationship between the entities defined
=end
  service('Locations Service') {
=begin
    The operation returns the list of UNSubDivision entites defined in the 
    SRD databases. If no record found and empty list should be returned.
=end
    operation('Get UNSubDivisions',         :returns => list('UNSubDivision')) {
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

=begin
    Returns a UNSubDivision as per the guid available in SRD according the parameters passed
    If no UNSubDivision available the method returns null
=end
    operation('Get UNSubDivision By GUID', :returns => 'UNSubDivision') {
      # The GUID for the UNSubDivision
      parameter 'guid',               :guid
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

=begin
    Returns a UNSubDivision as per the shortCode available in SRD according the parameters passed
    If no UNSubDivision available the method returns null
=end
    operation('Get UNSubDivision By Short Code', :returns => 'UNSubDivision') {
      # The short code for the Country
      parameter 'shortCode',          :string
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

#=================================================================================

=begin
    The operation returns the list of Country entites defined in the 
    SRD databases. If no record found and empty list should be returned.
=end
    operation('Get Countries',          :returns => list('Country')) {
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

=begin
    Returns a Country as per the guid available in SRD according the parameters passed
    If no Country available the method returns null
=end
    operation('Get Country By GUID', :returns => 'Country') {
      # The GUID for the Country
      parameter 'guid',               :guid
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

=begin
    Returns a Country as per the short code available in SRD according the parameters passed
    If no Country available the method returns null
=end
    operation('Get Country By Short Code', :returns => 'Country') {
      # The short code for the Country
      parameter 'shortCode',          :string
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

#=================================================================================

    # The operation returns the list of Logistics Locations entites defined in the 
    # SRD databases. If no record found and empty list should be returned.
    # If empty criteria passed then all the locations will be returned
    operation('Get Logistic Locations',          :returns => list('LogisticsLocation')) {
      # Filter criteria to narrow the result otherwise the full list will be returned
      parameter 'filterCriteria',     'LogLocCriteria'
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

    # Returns a Logistics Location as per the guid available in SRD according the parameters passed
    # If no Logistics Location available the method returns null
    operation('Get Logistics Location By GUID', :returns => 'LogisticsLocation') {
      # The GUID for the Logistics Location
      parameter 'guid',               :guid
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

    # Returns a Logistics Location as per the short code available in SRD according the parameters passed
    # If no Logistics Location available the method returns null
    operation('Get Logistics Location By Short Code', :returns => 'LogisticsLocation') {
      # The GUID for the Logistics Location
      parameter 'shortCode',          :string
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }


#=================================================================================

=begin
    The operation returns the list of UNLocation entites defined in the 
    SRD databases. If no record found and empty list should be returned.
=end
    operation('Get UNLocations',         :returns => list('UNLocation')) {
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

=begin
    Returns a UNLocation as per the guid available in SRD according the parameters passed
    If no UNLocation available the method returns null
=end
    operation('Get UNLocation By GUID', :returns => 'UNLocation') {
      # The GUID for the UNLocation
      parameter 'guid',               :guid
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

=begin
    Returns a UNLocation as per the short code available in SRD according the parameters passed
    If no UNLocation available the method returns null
=end
    operation('Get UNLocation By Short Code', :returns => 'UNLocation') {
      # The short code for the Country
      parameter 'shortCode',          :string
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

#=================================================================================

=begin
    The operation returns the relationship list between the Country and UNLocation
    The parent is 'Country' GUID
    The children are 'UNLocation' GUIDs
=end
    operation('Get Country Relationship To UNLocation', :returns => list('EntityRelationship')) {
    }

 #=================================================================================

=begin
    The operation returns the list of ConcentrationLevel entites defined in the
    SRD databases. If no record found and empty list should be returned.
=end
    operation('Get Concentration Levels', :returns => list('ConcentrationLevel')) {
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

=begin
    Returns a Concentration Level as per the guid available in SRD according the parameters passed
    If no Concentration Level available the method returns null
=end
    operation('Get Concentration Level By GUID', :returns => 'ConcentrationLevel') {
      # The GUID for the Concentration Level
      parameter 'guid',               :guid
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

=begin
    Returns a Concentration Level as per the level available in SRD according the parameters passed
    If no Concentration Level available the method returns null
=end
    operation('Get Concentration Level By Level', :returns => 'ConcentrationLevel') {
      # The short code for the Concentration Level
      parameter 'level',          :integer
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }
  }
}
