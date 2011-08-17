in_namespace('inner.security.departments') {

  define('RdDepartmentFlag', :extends => 'RdLookup') {
    constant 'allow all books', "ALLBK"
    constant 'allow spec books', "SPECB"
  }

  define('RdDepartmentFlagContainer', :extends => 'RdEntity') {
    field 'departmentId', :string
    field 'flagId', :string
    field 'modifiedBy', :string
  }

  # Maps to DEPARTMENTS table
  define('RdDepartment', :extends => 'RdEntity') {
    field 'name', :string
    field 'description', :string
    field 'modifiedBy', :string
    field 'statusId', :string
    field 'flags', :list, :element_type => 'RdDepartmentFlagContainer'
    field 'relatedDepartmentGroupCompanies', :list, :element_type => 'RdDepartmentGroupCompany'
  }

  # Maps to DGC_RELATED_GC table
  define('RdDepartmentGroupCompanyRelationship', :extends => 'RdEntity') {
    field 'departmentGCId', :string
    field 'groupCompanyId', :string
    field 'modifiedBy', :string
  }

  # Maps to DEPARTMENT_GROUP_COMPANIES table
  define('RdDepartmentGroupCompany', :extends => 'RdEntity') {
    field 'departmentId', :string
    field 'groupCompanyId', :string
    field 'businessLineId', :string
    field 'statusId', :string
    field 'modifiedBy', :string
    field 'departmentGroupCompanyRelationships', :list, :element_type => 'RdDepartmentGroupCompanyRelationship'
  }

  define('HDepartmentFlagContainer', :extends => 'RdDepartmentFlagContainer') {
    field 'verId', :string
    field 'verStart', :datetime
  }

  define('HDepartment', :extends => 'inner.security.departments.RdDepartment') {
    field 'verId', :string
    field 'verStart', :datetime
  }

  define('HDepartmentGroupCompany', :extends => 'RdDepartmentGroupCompany') {
    field 'verId', :string
    field 'verStart', :datetime
  }

  define('HDepartmentGroupCompanyRelationship', :extends => 'RdDepartmentGroupCompanyRelationship') {
    field 'verId', :string
    field 'verStart', :datetime
  }

}