in_namespace('inner.security.roles') {
=begin
  Roles Service
=end
  service('Roles Service') {

      # Returns list of Roles (not a versioned entity) In case there are no records in the databases empty list is returned.
    operation('Get List', :returns => list('RdRole')) {
      parameter 'statusCode', :string
    }

    operation('Get Full List', :returns => list('RdRole')) {
    }

    operation('Get By Id', :returns => 'RdRole') {
      parameter 'keyId', :string
    }

    operation('Save or Update Role', :returns => 'RdRole') {
      parameter 'role', :RdRole
    }

    operation('Get History', :returns => list('RdRole')) {
      parameter 'keyId', :string
    }

    operation('Get Associated Users', :returns => list('RdUser')) {
      parameter 'keyId', :string
    }

    operation('Get Conflicting Related Permissions', :returns => list('RdRelatedPermissions')) {
      parameter 'permissions', :list, :element_type => 'RdRolePermission'
    }
  }
}