#// This is the Permission service interface
in_namespace('TradeCapture.Internal.PermissionService') {

  #/// Permission Management Service
  service('PermissionService') {

    #/// permission checking/retrieval functions
    operation('GetAll', :returns => list('TradeCapture.Internal.PermissionService.Permission')) {
    }

    operation('GetAllUser', :returns => list('TradeCapture.Internal.PermissionService.Permission')) {
      parameter 'sid', :string
    }

    operation('GetEffective', :returns => list('TradeCapture.Internal.PermissionService.Permission')) {
      parameter 'sid', :string
      parameter 'requiredPermissions' , list('TradeCapture.Internal.PermissionService.Permission')
    }

    operation('CheckPermission', :returns => :boolean) {
      parameter 'sid', :string
      parameter 'requiredPermissions' , list('TradeCapture.Internal.PermissionService.Permission')
    }

    operation('CheckPermissionForRequester', :returns => :boolean) {
      parameter 'requiredPermissions' , list('TradeCapture.Internal.PermissionService.Permission')
    }

    #/// Permission management
    #/// Create a new permission
    operation('Create', :returns => :'TradeCapture.Internal.PermissionService.Permission') {
      parameter 'permission', :'TradeCapture.Internal.PermissionService.Permission'
    }

    #/// Delete a new permission
    operation('Delete', :returns => :boolean) {
      parameter 'permission', :'TradeCapture.Internal.PermissionService.Permission'
    }
  }
  
  service('DesktopPermissionService') {
    operation('GetTradersAndTrafficOperators', :returns => list('TrafficOperatorAndTradersForGC')) {
      parameter 'gcs' , list(:string)
    }  
  }

  #/// User Management Service
  service('UserService') {

    operation('Create', :returns => :PermissionResult) {
      parameter 'user', :'TradeCapture.Internal.PermissionService.User'
    }

    #/// get by sid
    operation('GetBySid', :returns => :'TradeCapture.Internal.PermissionService.User') {
      parameter 'sid', :string
    }

    #/// get by userId
    operation('GetByUserId', :returns => :'TradeCapture.Internal.PermissionService.User') {
      parameter 'userId', :UserId
    }

    #/// get by oid
    operation('Get', :returns => :'TradeCapture.Internal.PermissionService.User') {
      parameter 'oid', :integer
    }

    operation('TryGet', :returns => list('TradeCapture.Internal.PermissionService.User')) {
      parameter 'oid', :integer
    }

    operation('GetAll', :returns => list('TradeCapture.Internal.PermissionService.User')) {
    }

    operation('Update', :returns => :PermissionResult) {
      parameter 'user', :'TradeCapture.Internal.PermissionService.User'
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

    operation('GetUserRoles', :returns => list('TradeCapture.Internal.PermissionService.Role')) {
      parameter 'oid', :integer
    }

    operation('GetUserGroups', :returns => list('TradeCapture.Internal.PermissionService.Group')) {
      parameter 'oid', :integer
    }

    operation('GetUserGroupRoles', :returns => list('TradeCapture.Internal.PermissionService.Role')) {
      parameter 'oid', :integer
    }
  }

  #/// Group Management Service
  service('GroupService') {

    operation('Create', :returns => :PermissionResult) {
      parameter 'group', :Group
    }

    operation('Get', :returns => :'TradeCapture.Internal.PermissionService.Group') {
      parameter 'oid', :integer
    }

    operation('GetAll', :returns => list('TradeCapture.Internal.PermissionService.Group')) {
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

    operation('GetGroupUsers', :returns => list('TradeCapture.Internal.PermissionService.User')) {
      parameter 'oid', :integer
    }

    operation('GetGroupRoles', :returns => list('TradeCapture.Internal.PermissionService.Role')) {
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
      parameter 'role', :'TradeCapture.Internal.PermissionService.Role'
    }

    operation('Get', :returns => :'TradeCapture.Internal.PermissionService.Role') {
      parameter 'oid', :integer
    }

    operation('GetAll', :returns => list('TradeCapture.Internal.PermissionService.Role')) {
    }

    operation('Update', :returns => :PermissionResult) {
      parameter 'role', :'TradeCapture.Internal.PermissionService.Role'
    }

    operation('Delete', :returns => :boolean) {
      parameter 'oid', :integer
    }

    operation('GetRoleGroups', :returns => list('TradeCapture.Internal.PermissionService.Group')) {
      parameter 'oid', :integer
    }

    #/// Permission Role association management
    #/// Add a permission role association
    operation('AddPermissionRole', :returns => :PermissionRole) {
      parameter 'permission', :Permission
      parameter 'role', :'TradeCapture.Internal.PermissionService.Role'
    }

    #/// Remove a permission role association
    operation('RemovePermissionRole', :returns => :boolean) {
      parameter 'permission', :Permission
      parameter 'role', :Role
    }

    operation('GetRoleUsers', :returns => list('TradeCapture.Internal.PermissionService.User')) {
      parameter 'oid', :integer
    }

    operation('GetDirectRoleUsers', :returns => list('TradeCapture.Internal.PermissionService.User')) {
      parameter 'oid', :integer
    }
  }
}
