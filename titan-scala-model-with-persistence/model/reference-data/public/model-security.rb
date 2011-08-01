# EDM package EDM.Enterprise.Common Domains.Security
in_namespace('commondomains.referencedata.masterdata.security') {

=begin
  Permission entity - as an extension over Enumerated Item
  This entity maps to the PERMISSIONS table
=end
  define('Permission', :extends => 'EnumeratedItem') {
      # Maps to the STATUSES table via the STATUS_ID
    field 'status', 'Status'
    # Maps via MODULE_ID field to CODE_LOOKUPS table
    field 'module', :string
    # PURPOSE
    field 'purpose', :string
    # Maps via PERMISSION_FLAGS to CODE_LOOKUPS table
    field 'requiresApproval', :boolean
    # Maps via PERMISSION_FLAGS to CODE_LOOKUPS table
    field 'bookOverridePossible', :boolean
    # Maps via PERMISSION_FLAGS to CODE_LOOKUPS table
    field 'hasRoleSpecificValue', :boolean
    # Maps via PERMISSION_FLAGS to CODE_LOOKUPS table
    field 'treasuryPaymentPermission', :boolean
    # Association - Maps via PERMISSION_BUSINESS_OWNER to USERS
    field 'ownership', 'BusinessOwnership'
  # segregation : Association - Maps to PERMISSION_PERMISSIONS
  # field 'permissions', :list, :element_type => 'Permission'
  # field 'users', :list, :element_type => 'User'
  }

=begin
  Represents the granting of a permission to a role.
  Maps to ROLE_PERMISSIONS
=end
  define('RolePermission') {
    field 'value', 'string'
    field 'permission', 'Permission'
  }

=begin
  Role entity - as an extension over Enumerated Item
  This entity maps to the ROLES table
=end
  define('Role', :extends => 'EnumeratedItem') {
      # PURPOSE
    field 'purpose', :string
    # REQUIRES_APPROVAL
    field 'requiresApproval', :boolean
    # Maps to the STATUSES table via the STATUS_ID
    field 'status', 'Status'
    # Association - Maps via ROLE_PERMISSIONS to PERMISSIONS
    field 'permissions', :list, :element_type => 'RolePermission'
    # Association - Maps via ROLE_BUSINESS_OWNERS to USERS
    field 'ownership', 'BusinessOwnership'
  }

=begin
  Group entity - synonym to Role
=end
  define('Group', :extends => 'commondomains.referencedata.masterdata.security.Role') {}

=begin
  Department entity - as an extension over Enumerated Item
  This entity maps to the DEPARTMENTS table
=end
  define('Department', :extends => 'EnumeratedItem') {
      # Maps to the STATUSES table via the STATUS_ID
    field 'status', 'Status'
    # Association - Maps via DEPARTMENT_ROLES to ROLES
    field 'roles', :list, :element_type => 'commondomains.referencedata.masterdata.security.Role'
    # Association - Maps via DEPARTMENT_GROUP_COMPANIES to LE_GROUP_COMPANIES
    field 'relatedGroupCompanies', :list, :element_type => 'DepartmentOperation'
  }

=begin
  For a department linked to the group company in which it is based,
  an associated instance of Group Company Department grants a user access
  to a set of related group companies, as follows:
  If the user is assigned (directly or indirectly via a user group)
  to the specified department and the user is employed by
  the specified group company, then the user may access
  information relating to that group company and to the other related group companies.
  Maps to DEPARTMENT_GROUP_COMPANIES
=end
  define('DepartmentOperation') {
    field 'department', 'Department'
    field 'groupCompany', 'commondomains.referencedata.masterdata.legalentities.GroupCompany'
  }

=begin
  Job entity - synonym to Department
=end
  define('Job', :extends => 'Department') {}

=begin
  User entity - as an extension over Enumerated Item
  This entity maps to the USERS table
=end
  define('User', :extends => 'EnumeratedItem') {
      # FORENAME
    field 'forename', :string
    # SURNAME
    field 'surname', :string
    # Maps via USERS.PROFILE_STATUS_ID to CODE_LOOKUPS table
    field 'status', 'Status'
    # AD_USERNAME
    field 'activeDirectoryUsername', :string
    # AD_UID
    field 'activeDirectoryUID', :string
    # EMAIL_ADDRESS
    field 'emailAddress', :string
    # AD_SID
    field 'activeDirectorySID', :string
    # Maps via USER_FLAGS to CODE_LOOKUPS table
    field 'accessAllBooks', :boolean
    # Maps via USER_FLAGS to CODE_LOOKUPS table
    field 'accessAllCounterparties', :boolean
    # Maps via USER_FLAGS to CODE_LOOKUPS table
    field 'accessAllGroupCoBankAccounts', :boolean
    # Maps via USER_CITIZENSHIPS to COUNTRIES table
    field 'citizenships', :list, :element_type => 'Country'
    # Association - Maps via USER_DEPARTMENTS to DEPARTMENTS table
    field 'departments', :list, :element_type => 'Department'
    # Association - Maps via USER_ROLES to ROLES table
    field 'roles', :list, :element_type => 'commondomains.referencedata.masterdata.security.Role'
    # Association - Maps via USER_USERS to USERS table
    field 'userRelationships', :list, :element_type => 'UserRelationship'
    # Association - Maps via USER_LEGAL_ENTITIES to LEGAL_ENTITIES table
    field 'groupCompanies', :list, :element_type => 'RelationshipType'
    # Maps to USER_BOOK_PERMISSION_OVERRIDES
    field 'permissionsOverride', :list, :element_type => 'PermissionOverride'
  }

=begin
  UserCredentials entity
=end
  define('UserCredentials', :extends => 'Entity') {
    field 'books', :list, :element_type => 'Book'
    field 'citizenships', :list, :element_type => 'Country'
    field 'businessLines', :list, :element_type => 'BusinessLine'
    field 'counterparties', :list, :element_type => 'commondomains.referencedata.masterdata.legalentities.Company'
    field 'bankAccounts', :list, :element_type => 'GroupBankAccount'
    field 'employingGroupCompany', 'commondomains.referencedata.masterdata.legalentities.GroupCompany'
    field 'accessibleGroupCompanies', :list, :element_type => 'commondomains.referencedata.masterdata.legalentities.GroupCompany'
    field 'groupCompanyPermissions', :list, :element_type => 'GroupCompanyPermission'
    field 'userPermissions', :list, :element_type => 'UserPermission'
    field 'groupCompanyCountries', :list, :element_type => 'Country'
  }

=begin
  GroupCompanyPermission entity -
=end
  define('GroupCompanyPermission') {
    field 'departmentGroupCompany', 'commondomains.referencedata.masterdata.legalentities.GroupCompany'
  }

=begin
  UserPermission entity -
=end
  define('UserPermission') {
    field 'name', :string
    field 'deniedBooks', :list, :element_type => 'Book'
    field 'roleValues', :list, :element_type => 'RoleValue'
  }

=begin
  RoleValue entity -
=end
  define('RoleValue') {
    field 'role', 'commondomains.referencedata.masterdata.security.Role'
    field 'value', 'string'
  }

=begin
  A relationship between two users.
  Maps to USER_USERS
=end
  define('UserRelationship') {
      # Maps via RELATIONSHIP_ID field to CODE_LOOKUPS table
    field 'relationshipType', :string
    field 'user', 'commondomains.referencedata.masterdata.security.User'
  }

=begin
  Represents a permission for a User, overriding those granted
  by virtue of the User's Roles, in connection with a particular Book.
  Maps to USER_BOOK_PERMISSION_OVERRIDES
=end
  define('PermissionOverride') {
    field 'book', 'Book'
    field 'permission', 'Permission'
  }

=begin
  The type of relationship between a user and a group company.
  Maps to USER_LEGAL_ENTITIES
=end
  define('RelationshipType') {
    field 'name', :string
    field 'groupCompany', 'commondomains.referencedata.masterdata.legalentities.GroupCompany'
  }

=begin
  Encapsulates a ownership of an entity,
  consisting of a main owner and optionally one or more deputy owners.
=end
  define('BusinessOwnership', :extends => 'EnumeratedItem') {
    field 'ownershipType', :string
    field 'mainOwner', 'commondomains.referencedata.masterdata.security.User'
    field 'deputyOwners', :list, :element_type => 'commondomains.referencedata.masterdata.security.User'
  }

=begin
  Property entity -
  A piece of information of a particular type, used for restriction purposes.
=end
  define('Property', :extends => 'EnumeratedItem') {
    field 'entityType', :string
    field 'category', :string
    field 'value', :string
  }

=begin
  Restriction entity -
  The presence of an instance of Restriction says that a user having
  the attached property may not access an entity having the attached property.
=end
  define('Restriction', :extends => 'Entity') {
    field 'entityProperties', :list, :element_type => 'Property'
    field 'userProperties', :list, :element_type => 'Property'
  }

}
