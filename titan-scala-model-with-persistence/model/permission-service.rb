#// This is the Permission service interface
in_namespace('TradeMgmt.Internal.Permission') {

  #/// Permission Management Service
  service('PermissionService') {

    #/// permission checking/retrieval functions
    operation('GetAll', :returns => list('TradeMgmt.Internal.Permission.Permission')) {
    }

    operation('GetAllUser', :returns => list('TradeMgmt.Internal.Permission.Permission')) {
      parameter 'sid', :string
    }

    operation('GetEffective', :returns => list('TradeMgmt.Internal.Permission.Permission')) {
      parameter 'sid', :string
      parameter 'requiredPermissions' , list('TradeMgmt.Internal.Permission.Permission')
    }

    operation('CheckPermission', :returns => :boolean) {
      parameter 'sid', :string
      parameter 'requiredPermissions' , list('TradeMgmt.Internal.Permission.Permission')
    }

    operation('CheckPermissionForRequester', :returns => :boolean) {
      parameter 'requiredPermissions' , list('TradeMgmt.Internal.Permission.Permission')
    }

    #/// Permission management
    #/// Create a new permission
    operation('Create', :returns => :'TradeMgmt.Internal.Permission.Permission') {
      parameter 'permission', :'TradeMgmt.Internal.Permission.Permission'
    }

    #/// Delete a new permission
    operation('Delete', :returns => :boolean) {
      parameter 'permission', :'TradeMgmt.Internal.Permission.Permission'
    }
  }
  
  service('DesktopPermissionService') {
    operation('GetTradersAndTrafficOperators', :returns => list('TradersAndTrafOps')) {
      parameter 'gcs' , list(:string)
    }  
  }

  #/// User Management Service
  service('UserService') {

    operation('Create', :returns => :PermissionResult) {
      parameter 'user', :'TradeMgmt.Internal.Permission.User'
    }

    #/// get by sid
    operation('GetBySid', :returns => :'TradeMgmt.Internal.Permission.User') {
      parameter 'sid', :string
    }

    #/// get by userId
    operation('GetByUserId', :returns => :'TradeMgmt.Internal.Permission.User') {
      parameter 'userId', :UserId
    }

    #/// get by oid
    operation('Get', :returns => :'TradeMgmt.Internal.Permission.User') {
      parameter 'oid', :integer
    }

    operation('TryGet', :returns => list('TradeMgmt.Internal.Permission.User')) {
      parameter 'oid', :integer
    }

    operation('GetAll', :returns => list('TradeMgmt.Internal.Permission.User')) {
    }

    operation('Update', :returns => :PermissionResult) {
      parameter 'user', :'TradeMgmt.Internal.Permission.User'
    }

    operation('Delete', :returns => :boolean) {
      parameter 'oid', :integer
    }

    #/// Add/Remove roles
    operation('AddRole', :returns => :boolean) {
      parameter 'userOid', :integer
      parameter 'roleOid', :integer
    }

    operation('RemoveRole', :returns => :boolean) {
      parameter 'userOid', :integer
      parameter 'roleOid', :integer
    }

    operation('GetUserRoles', :returns => list('TradeMgmt.Internal.Permission.Role')) {
      parameter 'oid', :integer
    }

    operation('GetUserGroups', :returns => list('TradeMgmt.Internal.Permission.Group')) {
      parameter 'oid', :integer
    }

    operation('GetUserGroupRoles', :returns => list('TradeMgmt.Internal.Permission.Role')) {
      parameter 'oid', :integer
    }
  }

  #/// Group Management Service
  service('GroupService') {

    operation('Create', :returns => :PermissionResult) {
      parameter 'group', :Group
    }

    operation('Get', :returns => :'TradeMgmt.Internal.Permission.Group') {
      parameter 'oid', :integer
    }

    operation('GetAll', :returns => list('TradeMgmt.Internal.Permission.Group')) {
    }

    operation('Update', :returns => :PermissionResult) {
      parameter 'group', :Group
    }

    operation('Delete', :returns => :boolean) {
      parameter 'oid', :integer
    }

    #/// add/remove users from groups
    operation('AddUser', :returns => :boolean) {
      parameter 'groupOid', :integer
      parameter 'userOid', :integer
    }

    operation('RemoveUser', :returns => :boolean) {
      parameter 'groupOid', :integer
      parameter 'userOid', :integer
    }

    operation('GetGroupUsers', :returns => list('TradeMgmt.Internal.Permission.User')) {
      parameter 'oid', :integer
    }

    operation('GetGroupRoles', :returns => list('TradeMgmt.Internal.Permission.Role')) {
      parameter 'oid', :integer
    }

    #/// add/remove roles from groups
    operation('AddRole', :returns => :boolean) {
      parameter 'groupOid', :integer
      parameter 'roleOid', :integer
    }

    operation('RemoveRole', :returns => :boolean) {
      parameter 'groupOid', :integer
      parameter 'roleOid', :integer
    }
  }

  #/// Role Management Service
  service('RoleService') {

    operation('Create', :returns => :PermissionResult) {
      parameter 'role', :'TradeMgmt.Internal.Permission.Role'
    }

    operation('Get', :returns => :'TradeMgmt.Internal.Permission.Role') {
      parameter 'oid', :integer
    }

    operation('GetAll', :returns => list('TradeMgmt.Internal.Permission.Role')) {
    }

    operation('Update', :returns => :PermissionResult) {
      parameter 'role', :'TradeMgmt.Internal.Permission.Role'
    }

    operation('Delete', :returns => :boolean) {
      parameter 'oid', :integer
    }

    operation('GetRoleGroups', :returns => list('TradeMgmt.Internal.Permission.Group')) {
      parameter 'oid', :integer
    }

    #/// Permission Role association management
    #/// Add a permission role association
    operation('AddPermissionRole', :returns => :PermissionRole) {
      parameter 'permission', :Permission
      parameter 'role', :'TradeMgmt.Internal.Permission.Role'
    }

    #/// Remove a permission role association
    operation('RemovePermissionRole', :returns => :boolean) {
      parameter 'permission', :Permission
      parameter 'role', :Role
    }

    operation('GetRoleUsers', :returns => list('TradeMgmt.Internal.Permission.User')) {
      parameter 'oid', :integer
    }

    operation('GetDirectRoleUsers', :returns => list('TradeMgmt.Internal.Permission.User')) {
      parameter 'oid', :integer
    }
  }
}
