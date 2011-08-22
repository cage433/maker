# EDM package EDM.Enterprise.Common Domains.Reference Data.Enumerated Types
in_namespace('commondomains.referencedata.enumeratedtypes') {
=begin
  This service provides methods for retrieving entities defined in the
  Enumerated Types package along with methods which are returning the
  relationship between the entities defined
=end
  service('Enumerated Types Service') {
=begin
    The operation returns the list of Language entites defined in the 
    SRD databases. If no record found and empty list should be returned.
=end
    operation('Get Languages',         :returns => list('commondomains.referencedata.enumeratedtypes.Language')) {
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

=begin
    Returns a Language as per the guid available in SRD according the parameters passed
    If no Language available the method returns null
=end
    operation('Get Language By GUID', :returns => 'commondomains.referencedata.enumeratedtypes.Language') {
      # The GUID for the Language
      parameter 'guid',               :guid
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

=begin
    Returns a Language as per the short code available in SRD according the parameters passed
    If no Language available the method returns null
=end
    operation('Get Language By Short Code', :returns => 'commondomains.referencedata.enumeratedtypes.Language') {
      # The short code for the Language
      parameter 'shortCode',          :string
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

#=================================================================================

=begin
    The operation returns the list of VatCode entites defined in the 
    SRD databases. If no record found and empty list should be returned.
=end
    operation('Get Vat Codes',            :returns => list('VatCode')) {
    # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

=begin
    Returns a Vat Code as per the guid available in SRD according the parameters passed
    If no Vat Code available the method returns null
=end
    operation('Get Vat Code By GUID', :returns => 'VatCode') {
      # The GUID for the Vat Code
      parameter 'guid',               :guid
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

=begin
    Returns a Vat Code as per the short code available in SRD according the parameters passed
    If no Vat Code available the method returns null
=end
    operation('Get Vat Code By Short Code', :returns => 'VatCode') {
      # The short code for the Vat Code
      parameter 'shortCode',          :string
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

#=================================================================================

=begin
    The operation returns the list of Refined Metal Grades entites defined in the 
    SRD databases. If no record found and empty list should be returned.
=end
    operation('Get Refined Metal Grades',     :returns => list('RefinedMetalGrade')) {
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

=begin
    The operation returns the list of Refined Metal entites defined in the 
    SRD databases. If no record found and empty list should be returned.
=end
    operation('Get Refined Metals',     :returns => list('Commodity')) {
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

=begin
    The operation returns the list of Refined Metal Shapes entites defined in the 
    SRD databases. If no record found and empty list should be returned.
=end
    operation('Get Refined Metal Shapes',     :returns => list('RefinedMetalShape')) {
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

=begin
    The operation returns the list of Refined Metal Brands entites defined in the 
    SRD databases. If no record found and empty list should be returned.
=end
    operation('Get Refined Metal Brands',     :returns => list('RefinedMetalBrand')) {
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

#=================================================================================

=begin
    The operation returns the list of Location Function entites defined in the 
    SRD databases. If no record found and empty list should be returned.
=end
    operation('Get Location Functions',         :returns => list('LocationFunction')) {
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

=begin
    Returns a Location Function as per the guid available in SRD according the parameters passed
    If no Location Function available the method returns null
=end
    operation('Get Location Function By GUID', :returns => 'LocationFunction') {
      # The GUID for the LocationFunction
      parameter 'guid',               :guid
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

=begin
    Returns a Location Function as per the short code available in SRD according the parameters passed
    If no Location Function available the method returns null
=end
    operation('Get Location Function By Short Code', :returns => 'LocationFunction') {
      # The short code for the LocationFunction
      parameter 'shortCode',          :string
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

#=================================================================================

=begin
    The operation returns the list of Company Role entites defined in the 
    SRD databases. If no record found and empty list should be returned.
=end
    operation('Get Company Role',         :returns => list('CompanyRole')) {
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

=begin
    Returns a Company Role as per the guid available in SRD according the parameters passed
    If no Company Role available the method returns null
=end
    operation('Get Company Role By GUID', :returns => 'CompanyRole') {
      # The GUID for the Company Role
      parameter 'guid',               :guid
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

=begin
    Returns a Company Role as per the short code available in SRD according the parameters passed
    If no Company Role available the method returns null
=end
    operation('Get Company Role By Short Code', :returns => 'CompanyRole') {
      # The short code for the Company Role
      parameter 'shortCode',          :string
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

#=================================================================================

=begin
    The operation returns the list of COIDirection entites defined in the 
    SRD databases. If no record found and empty list should be returned.
=end
    operation('Get COI Directions', :returns => list('COIDirection')) {
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

=begin
    Returns a COIDirection as per the guid available in SRD according the parameters passed
    If no COIDirection available the method returns null
=end
    operation('Get COI Direction By GUID', :returns => 'COIDirection') {
      # The GUID for the COIDirection
      parameter 'guid',               :guid
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

=begin
    Returns a COIDirection as per the short code available in SRD according the parameters passed
    If no COIDirection available the method returns null
=end
    operation('Get COIDirection By Short Code', :returns => 'COIDirection') {
      # The short code for the COIDirection
      parameter 'shortCode',          :string
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }
#=================================================================================

=begin
    The operation returns the list of COIGranularity entites defined in the 
    SRD databases. If no record found and empty list should be returned.
=end
    operation('Get COI Granularities', :returns => list('COIGranularity')) {
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

=begin
    Returns a COIGranularity as per the guid available in SRD according the parameters passed
    If no COIGranularity available the method returns null
=end
    operation('Get COI Granularity By GUID', :returns => 'COIGranularity') {
      # The GUID for the COIGranularity
      parameter 'guid',               :guid
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

=begin
    Returns a COIGranularity as per the short code available in SRD according the parameters passed
    If no COIGranularity available the method returns null
=end
    operation('Get COI Granularity By Short Code', :returns => 'COIGranularity') {
      # The short code for the COIGranularity
      parameter 'shortCode',          :string
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }
#=================================================================================

=begin
    The operation returns the list of COIType entites defined in the 
    SRD databases. If no record found and empty list should be returned.
=end
    operation('Get COI Type', :returns => list('COIType')) {
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

=begin
    Returns a COIType as per the guid available in SRD according the parameters passed
    If no COIType available the method returns null
=end
    operation('Get COI Type By GUID', :returns => 'COIType') {
      # The GUID for the COIType
      parameter 'guid',               :guid
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

=begin
    Returns a COIType as per the short code available in SRD according the parameters passed
    If no COIType available the method returns null
=end
    operation('Get COI Type By Short Code', :returns => 'COIType') {
      # The short code for the COIType
      parameter 'shortCode',          :string
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }
#=================================================================================

=begin
    The operation returns the list of COICode entites defined in the 
    SRD databases. If no record found and empty list should be returned.
=end
    operation('Get COI Code', :returns => list('COICode')) {
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

=begin
    Returns a COICode as per the guid available in SRD according the parameters passed
    If no COICode available the method returns null
=end
    operation('Get COI Code By GUID', :returns => 'COICode') {
      # The GUID for the COICode
      parameter 'guid',               :guid
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

=begin
    Returns a COICode as per the short code available in SRD according the parameters passed
    If no COICode available the method returns null
=end
    operation('Get COI Code By Short Code', :returns => 'COICode') {
      # The short code for the COICode
      parameter 'shortCode',          :string
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

=begin
    The operation returns the list of TransportTypes entites defined in the 
    SRD databases. If no record found and empty list should be returned.
=end
    operation('Get Transport type', :returns => list('TransportType')) {
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

=begin
    Returns a TransportType as per the guid available in SRD according the parameters passed
    If no TransportType available exception is thrown
=end
    operation('Get TransportType By GUID', :returns => 'TransportType') {
      # The GUID for the TransportType
      parameter 'guid',               :guid
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

=begin
    Returns a TransportType as per the short code available in SRD according the parameters passed
    If no TransportType available exception is thrown
=end
    operation('Get Transport Type By Short Code', :returns => 'TransportType') {
      # The short code for the TransportType
      parameter 'shortCode',          :string
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

=begin
    The operation returns the list of OrderRequestTypes entites defined in the
    SRD databases. If no record found and empty list should be returned.
=end
    operation('Get Order Request Types', :returns => list('OrderRequestType')) {
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

=begin
    Returns a OrderRequestType as per the guid available in SRD according the parameters passed
    If no OrderRequestType available exception is thrown
=end
    operation('Get Order Request Type By GUID', :returns => 'OrderRequestType') {
      # The GUID for the OrderRequestType
      parameter 'guid',               :guid
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

=begin
    Returns a OrderRequestType as per the short code available in SRD according the parameters passed
    If no OrderRequestType available exception is thrown
=end
    operation('Get Order Request Type By Short Code', :returns => 'OrderRequestType') {
      # The short code for the OrderRequestType
      parameter 'shortCode',          :string
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

=begin
    The operation returns the list of OrderValidTimes entites defined in the
    SRD databases. If no record found and empty list should be returned.
=end
    operation('Get Order Valid Times', :returns => list('OrderValidTime')) {
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

=begin
    Returns a OrderValidTime as per the guid available in SRD according the parameters passed
    If no OrderValidTime available exception is thrown
=end
    operation('Get Order Valid Time By GUID', :returns => 'OrderValidTime') {
      # The GUID for the OrderValidTime
      parameter 'guid',               :guid
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

=begin
    Returns a OrderValidTime as per the short code available in SRD according the parameters passed
    If no OrderValidTime available exception is thrown
=end
    operation('Get Order Valid Time By Short Code', :returns => 'OrderValidTime') {
      # The short code for the OrderValidTime
      parameter 'shortCode',          :string
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

=begin
    The operation returns the list of OrderBasises entites defined in the
    SRD databases. If no record found and empty list should be returned.
=end
    operation('Get Order Basises', :returns => list('OrderBasis')) {
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

=begin
    Returns a OrderBasis as per the guid available in SRD according the parameters passed
    If no OrderBasis available exception is thrown
=end
    operation('Get Order Basis By GUID', :returns => 'OrderBasis') {
      # The GUID for the OrderBasis
      parameter 'guid',               :guid
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

=begin
    Returns a OrderBasis as per the short code available in SRD according the parameters passed
    If no OrderBasis available exception is thrown
=end
    operation('Get Order Basis By Short Code', :returns => 'OrderBasis') {
      # The short code for the OrderBasis
      parameter 'shortCode',          :string
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

=begin
    The operation returns the list of OrderOptionType entites defined in the
    SRD databases. If no record found and empty list should be returned.
=end
    operation('Get Order Option Types', :returns => list('OrderOptionType')) {
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

=begin
    Returns a OrderOptionType as per the guid available in SRD according the parameters passed
    If no OrderOptionType available exception is thrown
=end
    operation('Get Order Option Type By GUID', :returns => 'OrderOptionType') {
      # The GUID for the OrderOptionType
      parameter 'guid',               :guid
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

=begin
    Returns a OrderOptionType as per the short code available in SRD according the parameters passed
    If no OrderOptionType available exception is thrown
=end
    operation('Get Order Option Type By Short Code', :returns => 'OrderOptionType') {
      # The short code for the OrderOptionType
      parameter 'shortCode',          :string
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

=begin
    The operation returns the list of OrderOptionContractType entites defined in the
    SRD databases. If no record found and empty list should be returned.
=end
    operation('Get Order Option Contract Types', :returns => list('OrderOptionContractType')) {
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

=begin
    Returns a OrderOptionContractType as per the guid available in SRD according the parameters passed
    If no OrderOptionContractType available exception is thrown
=end
    operation('Get Order Option Contract Type By GUID', :returns => 'OrderOptionContractType') {
      # The GUID for the OrderOptionContractType
      parameter 'guid',               :guid
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

=begin
    Returns a OrderOptionContractType as per the short code available in SRD according the parameters passed
    If no OrderOptionContractType available exception is thrown
=end
    operation('Get Order Option Contract Type By Short Code', :returns => 'OrderOptionContractType') {
      # The short code for the OrderOptionContractType
      parameter 'shortCode',          :string
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

=begin
    The operation returns the list of Vessel entites defined in the
    SRD databases. If no record found and empty list should be returned.
=end
    operation('Get Vessels', :returns => list('Vessel')) {
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

=begin
    Returns a Vessel as per the guid available in SRD according the parameters passed
    If no Vessel available exception is thrown
=end
    operation('Get Vessel By GUID', :returns => 'Vessel') {
      # The GUID for the Vessel
      parameter 'guid',               :guid
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

=begin
    Returns a Vessel as per the short code available in SRD according the parameters passed
    If no Vessel available exception is thrown
=end
    operation('Get Vessel By Short Code', :returns => 'Vessel') {
      # The short code for the Vessel
      parameter 'shortCode',          :string
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

=begin
    The operation returns the list of DerivativeRight entites defined in the
    SRD databases. If no record found and empty list should be returned.
=end
    operation('Get DerivativeRights', :returns => list('DerivativeRight')) {
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

=begin
    Returns a DerivativeRight as per the guid available in SRD according the parameters passed
    If no DerivativeRight available exception is thrown
=end
    operation('Get DerivativeRight By GUID', :returns => 'DerivativeRight') {
      # The GUID for the DerivativeRight
      parameter 'guid',               :guid
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

=begin
    Returns a DerivativeRight as per the short code available in SRD according the parameters passed
    If no DerivativeRight available exception is thrown
=end
    operation('Get DerivativeRight By Short Code', :returns => 'DerivativeRight') {
      # The short code for the DerivativeRight
      parameter 'shortCode',          :string
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

=begin
    The operation returns the list of ExerciseType entites defined in the
    SRD databases. If no record found and empty list should be returned.
=end
    operation('Get ExerciseTypes', :returns => list('ExerciseType')) {
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

=begin
    Returns a ExerciseType as per the guid available in SRD according the parameters passed
    If no ExerciseType available exception is thrown
=end
    operation('Get ExerciseType By GUID', :returns => 'ExerciseType') {
      # The GUID for the ExerciseType
      parameter 'guid',               :guid
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

=begin
    Returns a ExerciseType as per the short code available in SRD according the parameters passed
    If no ExerciseType available exception is thrown
=end
    operation('Get ExerciseType By Short Code', :returns => 'ExerciseType') {
      # The short code for the ExerciseType
      parameter 'shortCode',          :string
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

=begin
    The operation returns the list of FXRule entites defined in the
    SRD databases. If no record found and empty list should be returned.
=end
    operation('Get FXRules', :returns => list('FXRule')) {
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

=begin
    Returns a FXRule as per the guid available in SRD according the parameters passed
    If no FXRule available exception is thrown
=end
    operation('Get FXRule By GUID', :returns => 'FXRule') {
      # The GUID for the FXRule
      parameter 'guid',               :guid
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

=begin
    Returns a FXRule as per the short code available in SRD according the parameters passed
    If no FXRule available exception is thrown
=end
    operation('Get FXRule By Short Code', :returns => 'FXRule') {
      # The short code for the FXRule
      parameter 'shortCode',          :string
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

=begin
    The operation returns the list of InterestRateType entites defined in the
    SRD databases. If no record found and empty list should be returned.
=end
    operation('Get InterestRateTypes', :returns => list('InterestRateType')) {
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

=begin
    Returns a InterestRateType as per the guid available in SRD according the parameters passed
    If no InterestRateType available exception is thrown
=end
    operation('Get InterestRateType By GUID', :returns => 'InterestRateType') {
      # The GUID for the InterestRateType
      parameter 'guid',               :guid
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

=begin
    Returns a InterestRateType as per the short code available in SRD according the parameters passed
    If no InterestRateType available exception is thrown
=end
    operation('Get InterestRateType By Short Code', :returns => 'InterestRateType') {
      # The short code for the InterestRateType
      parameter 'shortCode',          :string
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

=begin
    The operation returns the list of FundamentalUOM entites defined in the
    SRD databases. If no record found and empty list should be returned.
=end
    operation('Get FundamentalUOMs', :returns => list('commondomains.referencedata.enumeratedtypes.FundamentalUOM')) {
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

=begin
    Returns a FundamentalUOM as per the guid available in SRD according the parameters passed
    If no FundamentalUOM available exception is thrown
=end
    operation('Get FundamentalUOM By GUID', :returns => 'commondomains.referencedata.enumeratedtypes.FundamentalUOM') {
      # The GUID for the FundamentalUOM
      parameter 'guid',               :guid
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

=begin
    Returns a FundamentalUOM as per the short code available in SRD according the parameters passed
    If no FundamentalUOM available exception is thrown
=end
    operation('Get FundamentalUOM By Short Code', :returns => 'commondomains.referencedata.enumeratedtypes.FundamentalUOM') {
      # The short code for the FundamentalUOM
      parameter 'shortCode',          :string
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

=begin
    The operation returns the list of QPType entites defined in the
    SRD databases. If no record found and empty list should be returned.
=end
    operation('Get QPTypes', :returns => list('commondomains.referencedata.enumeratedtypes.QPType')) {
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

=begin
    Returns a QPType as per the guid available in SRD according the parameters passed
    If no QPType available exception is thrown
=end
    operation('Get QPType By GUID', :returns => 'commondomains.referencedata.enumeratedtypes.QPType') {
      # The GUID for the QPType
      parameter 'guid',               :guid
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

=begin
    Returns a QPType as per the short code available in SRD according the parameters passed
    If no QPType available exception is thrown
=end
    operation('Get QPType By Short Code', :returns => 'commondomains.referencedata.enumeratedtypes.QPType') {
      # The short code for the QPType
      parameter 'shortCode',          :string
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

=begin
    The operation returns the list of PaymentTerm entites defined in the
    SRD databases. If no record found and empty list should be returned.
=end
    operation('Get PaymentTerms', :returns => list('PaymentTerm')) {
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

=begin
    Returns a PaymentTerm as per the guid available in SRD according the parameters passed
    If no PaymentTerm available exception is thrown
=end
    operation('Get PaymentTerm By GUID', :returns => 'PaymentTerm') {
      # The GUID for the PaymentTerm
      parameter 'guid',               :guid
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

=begin
    Returns a PaymentTerm as per the short code available in SRD according the parameters passed
    If no PaymentTerm available exception is thrown
=end
    operation('Get PaymentTerm By Short Code', :returns => 'PaymentTerm') {
      # The short code for the PaymentTerm
      parameter 'shortCode',          :string
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

=begin
    The operation returns the list of DocumentType entites defined in the
    SRD databases. If no record found and empty list should be returned.
=end
    operation('Get DocumentTypes', :returns => list('commondomains.referencedata.enumeratedtypes.DocumentType')) {
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

=begin
    Returns a DocumentType as per the guid available in SRD according the parameters passed
    If no DocumentType available exception is thrown
=end
    operation('Get DocumentType By GUID', :returns => 'commondomains.referencedata.enumeratedtypes.DocumentType') {
      # The GUID for the DocumentType
      parameter 'guid',               :guid
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

=begin
    Returns a DocumentType as per the short code available in SRD according the parameters passed
    If no DocumentType available exception is thrown
=end
    operation('Get DocumentType By Short Code', :returns => 'commondomains.referencedata.enumeratedtypes.DocumentType') {
      # The short code for the DocumentType
      parameter 'shortCode',          :string
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

#=================================================================================

=begin
    The operation returns the relationship list between the Commodity and Refined Metal
    The parent is 'Refined Metal' GUID
    The children are 'Shape' GUIDs
    The physical table used for mapping is COMMODITY_SHAPES
=end
    operation('Get Refined Metal Relationship To Shape', :returns => list('EntityRelationship')) {
    }

=begin
    The operation returns the relationship list between the Commodity and Refined Metal
    The parent is 'Refined Metal' GUID
    The children are 'Brand' GUIDs
    The physical table used for mapping is COMMODITY_BRANDS
=end
    operation('Get Refined Metal Relationship To Brand', :returns => list('EntityRelationship')) {
    }

=begin
    The operation returns the relationship list between the Commodity and Refined Metal
    The parent is 'Refined Metal' GUID
    The children are 'Grade' GUIDs
    The physical table used for mapping is COMMODITY_GRADES
=end
    operation('Get Refined Metal Relationship To Grade', :returns => list('EntityRelationship')) {
    }
  }
}
