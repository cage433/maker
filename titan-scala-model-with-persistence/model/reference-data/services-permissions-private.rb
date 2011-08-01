in_namespace('inner.security.permissions') {
=begin
  Permissions Service
=end
  service('Permissions Service') {

    operation('Get List', :returns => list('RdPermission')) {
      parameter 'statusCode', :string
    }

    operation('Get Full List', :returns => list('RdPermission')) {
    }

    operation('Get By Id', :returns => 'RdPermission') {
      parameter 'keyId', :string
    }

    operation('Save or Update', :returns => 'RdPermission') {
      parameter 'permission', :RdPermission
    }

    operation('Get History', :returns => list('RdPermission')) {
      parameter 'keyId', :string
    }

    operation('Get Associated Users', :returns => list('RdUser')) {
      parameter 'keyId', :string
    }

  }

  service('Modules Service') {
    operation('Get List', :returns => list('RdModule')) {}
  }

  service('Permission Flags Service') {
    operation('Get List', :returns => list('RdPermissionFlag')) {}
  }

  service('Permission Relationship Service') {
    operation('Get List', :returns => list('RdPermissionRelationship')) {}
  }

}