in_namespace('inner.security.currencies') {

  service('Currencies Service') {

      # Returns list of Currency. In case there are no records in the databases empty list is returned.
      # Assuming the date as {now}:
      #    Get the current version if it exists
      #    Else, get the first (next) future version  if it exists
      #    Else, get the last expired version  if it exists
      #    Else, get the last expired and cancelled version if it exists
    operation('Get Version Independent List', :returns => list('RdCurrency')) {
    }

    # Create new or update existing Currency
    operation('Save or Update', :returns => 'RdCurrency') {
      parameter 'currency', :'RdCurrency'
    }

    # Returns all the versions of a given Currency as it's stored in the databases.
    operation('Get Versions List', :returns => list('RdCurrency')) {
      parameter 'keyId', :string
    }

    # Returns the given Currency by a key and version.
    operation('Get By Id', :returns => 'RdCurrency') {
      parameter 'keyId', :string
      parameter 'ver', :string
    }

    # Cancel Currency version
    operation('Cancel Version', :returns => list('RdCurrency')) {
      parameter 'keyId', :string
      parameter 'ver', :string
    }

    # Returns all Minor Units
    operation('Get Minor Unit List', :returns => list('RdMinorUnit')) {
    }
  }
}