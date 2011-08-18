in_namespace('inner.referencedata.concentrations') {

  service('Concentration Limits Service') {
    # return active RdConcentrationLimit entity by recId
    operation('Get By Rec Id', :returns => 'inner.referencedata.concentrations.RdConcentrationLimit') {
      parameter 'recId', :string
    }

    #Returns all concentration limits without parameters
    operation('Get Full List', :returns => list('inner.referencedata.concentrations.RdConcentrationLimit')) {
    }

    # Returns all the versions of a given Concentration Limit as it's stored in the databases. For versioning please see Reference Data SAD
    operation('Get Versions List', :returns => list('inner.referencedata.concentrations.RdConcentrationLimit')) {
      parameter 'keyId', :string
    }

    # Returns the given RdConcentrationLimit by a key and version.
    operation('Get By Id', :returns => 'inner.referencedata.concentrations.RdConcentrationLimit') {
      parameter 'keyId', :string
      parameter 'ver', :string
    }

    # Cancel Version
    operation('Cancel Version') {
      parameter 'keyId', :string
      parameter 'ver', :string
    }

=begin
    Persists the Concentration Limit in the database. For error conditions please see Reference Data SAD.
    If the saving was successful the service initiates the following CRUD events:
    If the save was invoked for a new record an 'Insert' event is initiated
    If the save was invoked for an existing record an 'Update' event is initiated
    For event details please see Reference Data SAD
=end
    operation('Save', :returns => 'inner.referencedata.concentrations.RdConcentrationLimit') {
      parameter 'cLimit', :'inner.referencedata.concentrations.RdConcentrationLimit'
    }
  }

  service('Concentration Levels Service') {
      # return active RdConcentrationLevel entity by ID. Assums atdate is NOW if it is null
    operation('Get By Id', :returns => 'inner.referencedata.concentrations.RdConcentrationLevel') {
      parameter 'keyId', :string
      parameter 'atDate', :datetime
    }

    #Returns all active concentration levels of certain type ordered by level ASC
    operation('Get By Type', :returns => list('inner.referencedata.concentrations.RdConcentrationLevel')) {
      parameter 'levelType', :'inner.referencedata.concentrations.RdConcentrationLevelType'
      parameter 'atDate', :datetime
    }

     # Returns all the versions of a given Concentration Level as it's stored in the databases. For versioning please see Reference Data SAD
    operation('Get Versions List', :returns => list('inner.referencedata.concentrations.RdConcentrationLevel')) {
      parameter 'keyId', :string
    }
  }

}