in_namespace('inner.security.users') {
=begin
   Users Service
=end

  service('Users Service') {
    operation('Get List', :returns => list('RdUser')) {
      parameter 'statusCode', :string
    }

    operation('Get Full List', :returns => list('RdUser')) {
    }

    operation('Get By Id', :returns => 'RdUser') {
      parameter 'keyId', :string
    }

    operation('Save or Update', :returns => 'RdUser') {
      parameter 'user', :RdUser
    }

    operation('Get History', :returns => list('RdUser')) {
      parameter 'keyId', :string
    }

    operation('Get Conflicting Related Permissions', :returns => list('RdRelatedPermissions')) {
      parameter 'user', :RdUser
    }

    operation('Get Inactive Departments', :returns => list('inner.security.departments.RdDepartment')) {
      parameter 'user', :RdUser
    }

    operation('Get Inactive Roles', :returns => list('RdRole')) {
      parameter 'user', :RdUser
    }

    operation('All Books Department Exists', :returns => :boolean) {
      parameter 'user', :RdUser
    }

    operation('Spec Books Department Exists', :returns => :boolean) {
      parameter 'user', :RdUser
    }

    operation('Get Book Override Changed Permissions', :returns => list('RdPermission')) {
      parameter 'permissions', :list, :element_type => 'RdPermission'
    }

    operation('Get Basic Users Full List', :returns => list('RdBasicUser')) {
    }

    # Synchronize users data in DB with AD
    operation('Sync With AD') {
    }

  }

  service('User Lookup Service') {
    operation('Get User Profile Statuses', :returns => list('RdUserProfileStatus')) {}
    operation('Get Business Owner Types', :returns => list('RdBusinessOwnerType')) {}
    operation('Get UserLE Relationships', :returns => list('RdUserLERelationship')) {}
    operation('Get User Relationships', :returns => list('RdUserRelationship')) {}
    operation('Get User Flags', :returns => list('RdUserFlag')) {}
  }

}