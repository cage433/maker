# EDM package EDM.Enterprise.Common Domains.Security
in_namespace('commondomains.referencedata.masterdata.security') {

=begin
  This service provides methods for retrieving entities defined in the
  security package along with methods which are returning the
  relationship between the entities defined
=end
  service('Security Service') {

=begin
  The operation returns the list of type Users
    as per the EDM (with business context)
=end
    operation('Get Users',               :returns => list('commondomains.referencedata.masterdata.security.User')) {
      # Base method params
      parameter 'baseParams',             'BaseServiceParams'
    }

=begin
  The operation returns the User Credentials by user SID in AD
=end
    operation('Get User Credentials By ADSID',          :returns => 'UserCredentials') {
      # Base method params
      parameter 'baseParams',             'BaseServiceParams'
      #user Active Directory SID
      parameter 'user active directory SID',            :string
    }

    #=================================================================================

=begin
  The operation returns the list of type Departments
    as per the EDM (with business context)
=end
    operation('Get Departments',         :returns => list('Department')) {
      # Base method params
      parameter 'baseParams',             'BaseServiceParams'
    }

=begin
  The operation Returns single Department object
    as per the EDM (with business context),
    which GUID matches the passed GUID
=end
    operation('Get Department By GUID',    :returns => 'Department') {
      # The GUID for the Department
      parameter 'guid',                   :guid
      # Base method params
      parameter 'baseParams',             'BaseServiceParams'
    }

=begin
  The operation Returns single Department object
    as per the EDM (with business context),
    of the object which matches the passed name.
=end
    operation('Get Department By Short Code',  :returns => 'Department') {
      # The shortCode for the Department
      parameter 'shortCode',                   :string
      # Base method params
      parameter 'baseParams',             'BaseServiceParams'
    }

    #=================================================================================

=begin
  The operation returns the list of type Roles
    as per the EDM (with business context)
=end
    operation('Get Roles',         :returns => list('commondomains.referencedata.masterdata.security.Role')) {
      # Base method params
      parameter 'baseParams',             'BaseServiceParams'
    }

=begin
  The operation Returns single Role object
    as per the EDM (with business context),
    which GUID matches the passed GUID
=end
    operation('Get Role By GUID',    :returns => 'commondomains.referencedata.masterdata.security.Role') {
      # The GUID for the Role
      parameter 'guid',                   :guid
      # Base method params
      parameter 'baseParams',             'BaseServiceParams'
    }

=begin
  The operation Returns single Role object
    as per the EDM (with business context),
    of the object which matches the passed name.
=end
    operation('Get Role By Short Code',  :returns => 'commondomains.referencedata.masterdata.security.Role') {
      # The shortCode for the Role
      parameter 'shortCode',                   :string
      # Base method params
      parameter 'baseParams',             'BaseServiceParams'
    }

    #=================================================================================

=begin
  The operation returns the list of type Permissions
    as per the EDM (with business context)
=end
    operation('Get Permissions',          :returns => list('commondomains.referencedata.masterdata.security.Permission')) {
      # Base method params
      parameter 'baseParams',             'BaseServiceParams'
    }

=begin
  The operation Returns single Permission object
    as per the EDM (with business context),
    which GUID matches the passed GUID
=end
    operation('Get Permission By GUID',   :returns => 'commondomains.referencedata.masterdata.security.Permission') {
      # The GUID for the Permission
      parameter 'guid',                   :guid
      # Base method params
      parameter 'baseParams',             'BaseServiceParams'
    }

=begin
  The operation Returns single Permission object
    as per the EDM (with business context),
    of the object which matches the passed name.
=end
    operation('Get Permission By Short Code',  :returns => 'commondomains.referencedata.masterdata.security.Permission') {
      # The shortCode for the Permission
      parameter 'shortCode',                   :string
      # Base method params
      parameter 'baseParams',                  'BaseServiceParams'
    }

    #=================================================================================

=begin
    The operation returns the list of type Restrictions
      as per the EDM (with business context)
=end
    operation('Get Restrictions',      :returns => list('commondomains.referencedata.masterdata.security.Restriction')) {
      # Base method params
      parameter 'baseParams',             'BaseServiceParams'
    }

    #=================================================================================

  }
  
}