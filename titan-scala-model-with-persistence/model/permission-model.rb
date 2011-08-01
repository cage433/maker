# This is the Permission service data model as used for Release 1 of RMET

in_namespace('TradeCapture.Internal.PermissionService') {

  # Unique User identifier (e.g. AD SID)
  define('UserId') {
    field 'id', :string # , :business_key => true
  }

  define('NeptuneUser') {
    field 'name', :string
    field 'id', :string
    field 'userType', :string
  }

  # user entity
  define('User') {
    field 'oid', :integer, :identifier => true
    field 'userId', 'UserId'
    field 'name', :string # , :business_key => true
    field 'disabled', :boolean
    field 'deleted', :boolean
    field 'neptune', 'NeptuneUser'
    field 'effectivePermissions', :list, :element_type => 'Permission', :optional => true
    field 'effectivePermissionsSet', :boolean
  }

  define('PermissionResult') {
    field 'success', :boolean
    field 'id', :integer
    field 'message', :string
  }

  # group entity
  define('Group') {
    field 'oid', :integer, :identifier => true
    field 'name', :string # , :business_key => true
    field 'description', :string
    field 'disabled', :boolean
    field 'deleted', :boolean
  }
  
  # Role entity
  define('Role') {
    field 'oid', :integer, :identifier => true
    field 'name', :string # , :business_key => true
    field 'description', :string
    field 'disabled', :boolean
    field 'deleted', :boolean
  }

  # explicit many to many mapping tables
  # ideally the binding-gen dsl would support this but it needs implementing
  define('UserRoles') {
    field 'oid', :integer, :identifier => true
    field 'user', :integer_key, :references => 'TradeCapture.Internal.PermissionService.User(oid)'
    field 'role', :integer_key, :references => 'TradeCapture.Internal.PermissionService.Role(oid)'
  }

  define('GroupRoles') {
    field 'oid', :integer, :identifier => true
    field 'group', :integer_key, :references => 'TradeCapture.Internal.PermissionService.Group(oid)'
    field 'role', :integer_key, :references => 'TradeCapture.Internal.PermissionService.Role(oid)'
  }

  define('UserGroups') {
    field 'oid', :integer, :identifier => true
    field 'user', :integer_key, :references => 'TradeCapture.Internal.PermissionService.User(oid)'
    field 'group', :integer_key, :references => 'TradeCapture.Internal.PermissionService.Group(oid)'
  }

  # permission entity (permission name and oid)
  define('Permission') {
#    field 'oid', :integer, :identifier => true
    field 'name', :string, :business_key => true
  }

  # permission <-> role association
  define('PermissionRole') {
    field 'oid', :integer, :identifier => true
    field 'permission', :integer # :integer_key , :references => 'TradeCapture.Internal.PermissionService.Permission(oid)'
    field 'role', :integer_key, :references => 'TradeCapture.Internal.PermissionService.Role(oid)'
  }
  
    define('TrafficOperatorAndTradersForGC') {
    field 'gcCode',                     :string
    field 'traders',                   :list, :element_type => 'User', :optional => true
    field 'trafficOperators',          :list, :element_type => 'User', :optional => true
  }


}
