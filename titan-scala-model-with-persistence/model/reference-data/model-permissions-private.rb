in_namespace('inner.security.permissions') {

  define('RdModule', :extends => 'RdLookup') {
    constant 'Accounts', "ACCS"
    constant 'Finance', "FINAN"
    constant 'Reference Data', "REFDT"
    constant 'Titan Operations', "TOPER"
    constant 'Treasury', "TREAS"
    constant 'Titan Trades', "TTRAD"
  }

  define('RdPermissionFlag', :extends => 'RdLookup') {
    constant 'Requires approval', "APPRQ"
    constant 'Book override possible', "BKOVR"
    constant 'Role specific value', "ROLSV"
    constant 'Treasury payment permission', "TRSPP"
  }

  define('RdPermissionFlagContainer', :extends => 'RdEntity') {
    field 'permissionId', :string
    field 'flagId', :string
    field 'modifiedBy', :string
  }

  define('RdPermissionRelationship', :extends => 'RdLookup') {
    constant 'Exclusive', "EXCL"
  }

  # Maps to PERMISSION_BUSINESS_OWNER
  define('RdPermissionBusinessOwner', :extends => 'RdBusinessOwner') {
    field 'permissionId', :string
    field 'modifiedBy', :string
  }

  # Maps to PERMISSION_PERMISSIONS table
  define('RdRelatedPermissions', :extends => 'RdEntity') {
    field 'permissionId', :string
    field 'relatedPermissionId', :string
    field 'relationshipId', :string
    field 'modifiedBy', :string
  }

  # Maps to PERMISSIONS table
  define('RdPermission', :extends => 'RdEntity') {
    field 'moduleId', :string
    field 'name', :string
    field 'purpose', :string
    field 'modifiedBy', :string
    field 'statusId', :string
    field 'flags', :list, :element_type => 'RdPermissionFlagContainer'
    field 'businessOwners', :list, :element_type => 'RdPermissionBusinessOwner'
    field 'relatedPermissions', :list, :element_type => 'RdRelatedPermissions'
    field 'associatedRoles', :list, :element_type => 'RdRolePermission'
  }

  define('HPermissionBusinessOwner', :extends => 'RdPermissionBusinessOwner') {
    field 'verId', :string
    field 'verStart', :datetime
  }

  define('HPermissionFlagContainer', :extends => 'RdPermissionFlagContainer') {
    field 'verId', :string
    field 'verStart', :datetime
  }

  define('HRelatedPermissions', :extends => 'RdRelatedPermissions') {
    field 'verId', :string
    field 'verStart', :datetime
  }

  define('HPermission', :extends => 'RdPermission') {
    field 'verId', :string
    field 'verStart', :datetime
  }

}