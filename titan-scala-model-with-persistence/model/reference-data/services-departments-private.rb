in_namespace('inner.security.departments') {
=begin
  Departments Service
=end
  service('Departments Service') {
    # Returns list of Departments (not a versioned entity) In case there are no records in the databases empty list is returned.
    operation('Get List', :returns => list('inner.security.departments.RdDepartment')) {
      parameter 'statusCode',  :string
    }

    operation('Get Full List', :returns => list('inner.security.departments.RdDepartment')) {
    }

    operation('Get By Id', :returns => 'inner.security.departments.RdDepartment') {
      parameter 'keyId',      :string
    }

    operation('Save or Update Department', :returns => 'inner.security.departments.RdDepartment') {
      parameter 'department', :RdDepartment
    }

    operation('Get History', :returns => list('inner.security.departments.RdDepartment')) {
      parameter 'keyId',     :string
    }

    operation('Get Associated Users', :returns => list('RdUser')) {
      parameter 'keyId',     :string
    }

    operation('Get Associated Users To All Book', :returns => list('RdUser')) {
      parameter 'department', :RdDepartment
    }

    operation('Get Associated Users To Spec Book', :returns => list('RdUser')) {
      parameter 'department', :RdDepartment
    }

  }

  service('Department Flags Service') {
    operation('Get List', :returns => list('RdDepartmentFlag')) { }
  }

}