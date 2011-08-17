in_namespace('inner.ReferenceData.LegalEntities') {

  # Returns List of LegalEntities
  service('Legal Entity Service') {

    operation('Get Items Count', :returns => :integer) {
        parameter 'roleShortCode', :string
        parameter 'filterData',  'RdEntityFilter'
    }

    operation('Get Items', :returns => list('RdLegalEntity')) {
        parameter 'roleShortCode', :string
        parameter 'filterData',  'RdEntityFilter'
    }

    operation('Get Group Values And Counts', :returns => list('KeyValuePair')) {
        parameter 'roleShortCode', :string
        parameter 'filterData', 'RdEntityFilter'
        parameter 'groupPropertyName', :string
    }

    # Returns all Legal Entities by role
    operation('Get List', :returns => list('RdLegalEntity')) {
        parameter 'roleShortCode', :string
    }

    operation('Get Versions', :returns => list('RdLegalEntity')) {
          parameter 'role', :string
          parameter 'guid', :string
    }

    operation('Get By GUID', :returns => 'RdLegalEntity') {
          parameter 'role', :string
          parameter 'guid', :string
    }


    operation('Cancel Version') {
              parameter 'guid',    :string
              parameter 'ver',   :integer
    }

    operation('Save', :returns => 'RdLegalEntity') {
          parameter 'entity', :'RdLegalEntity'
    }

    # Returns all Banks Groups
    operation('Get Banks Groups List', :returns => list('RdLegalEntity')) { }

  }


  # Returns List of Le Bank Flags
  service('Le Bank Flag Service') {
    #Returns all flags
    operation('Get Full List', :returns => list('RdLeBankFlag')) {
    }
  }
   

  # Return List of  Location Function Types
  service('Location Function Types Service') {
    #Returns all Location Function Types
    operation('Get List', :returns => list('RdLocationFuncType')) {
    }
  }

  # Returns List of Notes
  service('Note Service') {
    # Returns all Notes
    operation('Get Notes By Parent GUID', :returns => list('RdNote')) {
            parameter 'parentGUID',  :string
    }

    operation('Save', :returns => 'RdNote') {
          parameter 'entity', :'RdNote'
    }

  }
  # Returns List of Le Roles
  service('Le Role Service') {
    #Returns all RdLegalEntityRole
    operation('Get Full List', :returns => list('RdLegalEntityRole')) {
    }
  }

}


