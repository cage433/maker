in_namespace('inner.security.books') {
=begin
  Books Service
=end
  service('Books Service') {

      # Returns list of Books. In case there are no records in the databases empty list is returned.
  # Assuming the date as {now}:
  #    Get the current version if it exists
  #    Else, get the first (next) future version  if it exists
  #    Else, get the last expired version  if it exists
  #    Else, get the last expired and cancelled version if it exists
    operation('Get Version Independent List', :returns => list('RdBook')) {
    }

    # Returns list of Books latest not canceled versions. In case there are no records in the databases empty list is returned.
    operation('Get Latest Books List', :returns => list('RdBook')) {
    }

    # Returns all the versions of a given Book as it's stored in the databases.
    operation('Get Versions List', :returns => list('RdBook')) {
      parameter 'keyId', :string
    }

    # Returns the given RdBook by a key and version.
    operation('Get By Id', :returns => 'RdBook') {
      parameter 'keyId', :string
      parameter 'ver', :string
    }

    operation('Create or Update Book', :returns => 'RdBook') {
      parameter 'book', :RdBook
    }

    operation('Cancel Book Version', :returns => list('RdBook')) {
      parameter 'book', :RdBook
    }

    operation('Get Spec Book Users', :returns => list('RdUser')) {
      parameter 'book', :RdBook
    }
  }

  service('Reporting Groups Service') {
    operation('Get List', :returns => list('RdReportingGroup')) {}
  }

  service('Book Groups Service') {
    operation('Get List', :returns => list('RdBookGroup')) {}
  }

  service('Book Statuses Service') {
    operation('Get List', :returns => list('RdBookStatus')) {}
  }

}