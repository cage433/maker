# EDM package EDM.Enterprise.Common Domains.Reference Data.Master Data.Legal Entities
in_namespace('commondomains.referencedata.masterdata.legalentities') {
=begin
  This service provides methods for retrieving entities defined in the
  Legal Entities package along with methods which are returning the
  relationship between the entities defined
=end
  service('Legal Entities Service') {
=begin
    Returns the list of Companies available in SRD according the parameters passed
    If no companies available an empty list is returned
=end
    operation('Get Companies', :returns => list('Company')) {
      # Company role short code
      parameter 'companyRoleShortCode',      :string
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

=begin
    Returns a Company as per the guid available in SRD according the parameters passed
    If no Company available the method returns null
=end
    operation('Get Company By GUID', :returns => 'Company') {
      # The GUID for the company
      parameter 'guid',               :guid
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

=begin
    Returns a Company by short code and company Role short code
    If no Company available the method returns null
=end
    operation('Get Company By Short Code', :returns => 'Company') {
      # Company short code
      parameter 'companyShortCode',   :string
      # Company role short code
      parameter 'companyRoleShortCode',   :string
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

#=================================================================================

=begin
    Returns the list of Exchanges available in SRD according the parameters passed
    If no exchanges available an empty list is returned
=end
    operation('Get Exchanges', :returns => list('commondomains.referencedata.masterdata.legalentities.Exchange')) {
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

=begin
    Returns Exchange as per the guid available in SRD according the parameters passed
    If no Exchange available exception is thrown
=end
    operation('Get Exchange By GUID', :returns => 'commondomains.referencedata.masterdata.legalentities.Exchange') {
      # The GUID for the Exchange
      parameter 'guid',               :guid
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

=begin
    Returns Exchange as per the short code available in SRD according the parameters passed
    If no Exchange available exception is thrown
=end
    operation('Get Exchange By Short Code', :returns => 'commondomains.referencedata.masterdata.legalentities.Exchange') {
      # The short code for the Exchange
      parameter 'shortCode',          :string
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

#=================================================================================


=begin
    Returns the list of Group Companies available in SRD according the parameters passed
    If no group companies available an empty list is returned
=end
    operation('Get Group Companies', :returns => list('commondomains.referencedata.masterdata.legalentities.GroupCompany')) {
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

=begin
    Returns a Group Company as per the guid available in SRD according the parameters passed
    If no Group Company available the method returns null
=end
    operation('Get Group Company By GUID', :returns => 'commondomains.referencedata.masterdata.legalentities.GroupCompany') {
      # The GUID for the company
      parameter 'guid',               :guid
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

=begin
    Returns a Group Company as per the short code available in SRD according the parameters passed
    If no Group Company available the method returns null
=end
    operation('Get Group Company By Short Code', :returns => 'commondomains.referencedata.masterdata.legalentities.GroupCompany') {
      # The short code for the Group Company
      parameter 'shortCode',          :string
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

#=================================================================================

=begin
    Returns the list of Contacts available in SRD according the parameters passed
    If no contacts available an empty list is returned
=end
    operation('Get Contacts', :returns => list('commondomains.referencedata.masterdata.legalentities.Contact')) {
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

=begin
    Returns a Contact as per the guid available in SRD according the parameters passed
    If no Contact available the method returns null
=end
    operation('Get Contact By GUID', :returns => 'commondomains.referencedata.masterdata.legalentities.Contact') {
      # The GUID for the company
      parameter 'guid',               :guid
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

#=================================================================================

=begin
    Returns a Bank Account as per the guid available in SRD according the parameters passed
    If no Bank Account available the method returns null
=end
    operation('Get Bank Account By GUID', :returns => 'BankAccount') {
      # The GUID for the company
      parameter 'guid',               :guid
      # Base method params
      parameter 'baseParams',         'BaseServiceParams'
    }

#=================================================================================

=begin
    The operation returns the relationship list between the Company and Contact
    The parent is 'Company' GUID
    The children are 'Contact' GUIDs
=end
    operation('Get Company Relation To Contact', :returns => list('EntityRelationship')) {
    }

=begin
    The operation returns the relationship list between the Company and Bank Account
    The parent is 'Company' GUID
    The children are 'Bank Account' GUIDs
=end
    operation('Get Company Relation To Bank Account', :returns => list('EntityRelationship')) {
    }

=begin
    The operation returns the relationship list between the Company and Company
    The parent is 'Company' GUID
    The children are 'Company' GUIDs
=end
    operation('Get Company Relation To Company', :returns => list('EntityRelationship')) {
    }

=begin
    The operation returns the relationship list between the Company and Group Company
    if a Company is a valid counterparty for the Group Company 
    The parent is 'Company' GUID
    The children are 'Group Company' GUIDs
    The physical table used for mapping is GROUP_COMPANY_MAPPING
=end
    operation('Get Counterparty Relationship To Group Company', :returns => list('EntityRelationship')) {
    }

=begin
    The operation returns the relationship list between the Group Company and Group
    if a Group Company has a valid counterparty for the Company 
    The parent is 'Group Company' GUID
    The children are 'Company' GUIDs
    The physical table used for mapping is GROUP_COMPANY_MAPPING
=end
    operation('Get Group Companies Relationship To Counterparties', :returns => list('EntityRelationship')) {
    }

=begin
    The operation returns the relationship list between the Company Role and Company
    The parent is 'Company Role' GUID
    The children are 'Company' GUIDs
=end
    operation('Get Company Role Relationship To Company', :returns => list('EntityRelationship')) {
    }

=begin
    The operation returns the relationship list between the Vat Code and Company
    The parent is 'Vat Code' GUID
    The children are 'Company' GUIDs
=end
    operation('Get Vat Code Relationship To Company', :returns => list('EntityRelationship')) {
    }
  }
}
