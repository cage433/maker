in_namespace('inner.security.users') {

  define('RdUserFlag', :extends => 'RdLookup') {
    constant 'allow all books', "ALBKS"
    constant 'is approver', "ISAPP"
    constant 'All Bank Accounts', "ALBA"
    constant 'All Counterparties', "ALCP"
  }

  define('RdBusinessOwnerType', :extends => 'RdLookup') {
    constant 'Main', "MAIN"
    constant 'Deputy', "DPTY"
  }

  define('RdUserProfileStatus', :extends => 'RdLookup') {
    constant 'Active', "ACT"
    constant 'Expired', "EXP"
    constant 'Inactive', "INACT"
    constant 'In Progress', "INPR"
  }

  define('RdUserRelationship', :extends => 'RdLookup') {
    constant 'Approved By', "APPR"
    constant 'Copy Of', "COPY"
  }

  define('RdUserLERelationship', :extends => 'RdLookup') {
    constant 'Accessible Counterparty', "ACP"
    constant 'Accessible Group Company', "AGC"
    constant 'Employing Group Company', "EGC"
  }

  define('RdBusinessOwner', :extends => 'RdEntity') {
    field 'userId', :string
    field 'ownerTypeId', :string
  }

  # Maps to USER_FLAGS table
  define('RdUserFlagContainer', :extends => 'RdEntity') {
    field 'userId', :string
    field 'flagId', :string
    field 'modifiedBy', :string
  }

  # Maps to USER_USERS table
  define('RdRelatedUser', :extends => 'RdEntity') {
    field 'userId', :string
    field 'relatedUserId', :string
    field 'relationshipId', :string
    field 'modifiedBy', :string
  }

  # Maps to USER_BUSINESS_LINES table
  define('RdUserBusinessLine', :extends => 'RdEntity') {
    field 'userId', :string
    field 'businessLineId', :string
    field 'modifiedBy', :string
  }

  # Maps to USER_CITIZENSHIPS table
  define('RdUserCountry', :extends => 'RdEntity') {
    field 'userId', :string
    field 'countryId', :string
    field 'modifiedBy', :string
  }

  # Maps to USER_DEPARTMENTS table
  define('RdUserDepartment', :extends => 'RdEntity') {
    field 'userId', :string
    field 'departmentId', :string
    field 'modifiedBy', :string
  }

  # Maps to USER_ROLES table
  define('RdUserRole', :extends => 'RdEntity') {
    field 'userId', :string
    field 'roleId', :string
    field 'modifiedBy', :string
  }

  # Maps to USER_BANK_ACCOUNTS table
  define('RdUserBankAccount', :extends => 'RdEntity') {
    field 'userId', :string
    field 'bankAccountId', :string
    field 'modifiedBy', :string
  }

  # Maps to USER_LEGAL_ENTITIES table
  define('RdUserLegalEntity', :extends => 'RdEntity') {
    field 'userId', :string
    field 'legalEntityId', :string
    field 'relationshipId', :string
    field 'modifiedBy', :string
  }

  define('RdUserGroupCompany', :extends => 'RdUserLegalEntity') {
  }

  define('RdUserCounterparty', :extends => 'RdUserLegalEntity') {
  }

  # Maps to USER_BOOK_PERMISSION_OVERRIDES table
  define('RdUserBookPermissionOverrides', :extends => 'RdEntity') {
    field 'userBookId', :string
    field 'permissionId', :string
    field 'modifiedBy', :string
  }

  # Maps to USER_BOOKS table
  define('RdUserBook', :extends => 'RdEntity') {
    field 'userId', :string
    field 'bookId', :string
    # field 'book',                      'RdBook'
    field 'userBookPermissionOverrides', :list, :element_type => 'RdUserBookPermissionOverrides'
    field 'modifiedBy', :string
  }

  # Maps to USERS table
  define('RdBasicUser', :extends => 'RdEntity') {
    field 'userName', :string
    field 'foreName', :string
    field 'surName', :string
    field 'emailAddress', :string
    field 'modifiedBy', :string
    field 'activeDirectoryUID', :string
    field 'activeDirectorySID', :string
    # Maps via USERS.PROFILE_STATUS_ID to CODE_LOOKUPS table
    field 'profileStatusId', :string
    field 'legalEntities', :list, :element_type => 'RdUserLegalEntity'
    field 'userGroupCompanies', :list, :element_type => 'RdUserGroupCompany'
    field 'counterparties', :list, :element_type => 'RdUserCounterparty'
    field 'roles', :list, :element_type => 'RdUserRole'
    field 'flags', :list, :element_type => 'RdUserFlagContainer'
  }

  # Maps to USERS table
  define('RdUser', :extends => 'RdBasicUser') {
    field 'relatedUsers', :list, :element_type => 'RdRelatedUser'
    field 'businessLines', :list, :element_type => 'RdUserBusinessLine'
    field 'countries', :list, :element_type => 'RdUserCountry'
    field 'departments', :list, :element_type => 'RdUserDepartment'
    field 'bankAccounts', :list, :element_type => 'RdUserBankAccount'
    field 'books', :list, :element_type => 'RdUserBook'
  }

  #History

  define('HUserLegalEntity', :extends => 'RdUserLegalEntity') {
    field 'verId', :string
    field 'verStart', :datetime
  }

  define('HUserFlagContainer', :extends => 'RdUserFlagContainer') {
    field 'verId', :string
    field 'verStart', :datetime
  }

  define('HRelatedUser', :extends => 'RdRelatedUser') {
    field 'verId', :string
    field 'verStart', :datetime
  }

  define('HUserBusinessLine', :extends => 'RdUserBusinessLine') {
    field 'verId', :string
    field 'verStart', :datetime
  }

  define('HUserCountry', :extends => 'RdUserCountry') {
    field 'verId', :string
    field 'verStart', :datetime
  }

  define('HUserDepartment', :extends => 'RdUserDepartment') {
    field 'verId', :string
    field 'verStart', :datetime
  }

  define('HUserRole', :extends => 'RdUserRole') {
    field 'verId', :string
    field 'verStart', :datetime
  }

  define('HUserBankAccount', :extends => 'RdUserBankAccount') {
    field 'verId', :string
    field 'verStart', :datetime
  }

  define('HUserBook', :extends => 'RdUserBook') {
    field 'verId', :string
    field 'verStart', :datetime
  }

  define('HUserBookPermissionOverrides', :extends => 'RdUserBookPermissionOverrides') {
    field 'verId', :string
    field 'verStart', :datetime
  }

  define('HUser', :extends => 'RdUser') {
    field 'verId', :string
    field 'verStart', :datetime
  }

}