in_namespace('inner.security.roles') {

    # Maps to ROLE_BUSINESS_OWNERS
  define('RdRoleBusinessOwner', :extends => 'RdBusinessOwner') {
    field 'roleId', :string
    field 'modifiedBy', :string
  }

  # Maps to ROLE_PERMISSIONS
  define('RdRolePermission', :extends => 'RdEntity') {
    field 'roleId', :string
    field 'permissionId', :string
    field 'specificValue', :string
    field 'modifiedBy', :string
  }

  # Maps to DEPARTMENT_ROLES
  define('RdDepartmentRole', :extends => 'RdEntity') {
    field 'roleId', :string
    field 'departmentId', :string
    field 'modifiedBy', :string
  }

  # Maps to ROLES table
  define('RdRole', :extends => 'RdEntity') {
    field 'name', :string
    field 'purpose', :string
    field 'requiresApproval', :boolean
    field 'statusId', :string
    field 'modifiedBy', :string
    field 'businessOwners', :list, :element_type => 'RdRoleBusinessOwner'
    field 'relatedPermissions', :list, :element_type => 'RdRolePermission'
    field 'relatedDepartments', :list, :element_type => 'RdDepartmentRole'
  }

  define('HRole', :extends => 'RdRole') {
    field 'verId', :string
    field 'verStart', :datetime
  }

  define('HRolePermission', :extends => 'RdRolePermission') {
    field 'verId', :string
    field 'verStart', :datetime
  }

  define('HRoleBusinessOwner', :extends => 'RdRoleBusinessOwner') {
    field 'verId', :string
    field 'verStart', :datetime
  }

  define('HDepartmentRole', :extends => 'RdDepartmentRole') {
    field 'verId', :string
    field 'verStart', :datetime
  }

}