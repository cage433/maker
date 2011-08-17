in_namespace('inner.ReferenceData.Commodities') {

  # Commodity Service
  service('Commodity Service') {
    operation('Get List', :returns => list('RdRefinedMetal')) {
    }

    operation('Get Versions', :returns => list('RdRefinedMetal')) {
      parameter 'guid', :string
    }

    operation('Get By GUID', :returns => 'RdRefinedMetal') {
      parameter 'guid', :string
    }

    operation('Get By Short Code', :returns => 'RdRefinedMetal') {
      parameter 'shortCode', :string
    }

    operation('Cancel Version') {
      parameter 'guid', :string
      parameter 'ver', :integer
    }

    operation('Save', :returns => 'RdRefinedMetal') {
      parameter 'entity', :RdRefinedMetal
    }

  }

  # Grade Service
  service('Grade Service') {
    operation('Get List', :returns => list('RdRefinedMetalGrade')) {
    }

    operation('Get Versions', :returns => list('RdRefinedMetalGrade')) {
      parameter 'guid', :string
    }

    operation('Get By GUID', :returns => 'RdRefinedMetalGrade') {
      parameter 'guid', :string
    }

    operation('Get By Short Code', :returns => 'RdRefinedMetalGrade') {
      parameter 'shortCode', :string
    }

    operation('Cancel Version') {
      parameter 'guid', :string
      parameter 'ver', :integer
    }

    operation('Save', :returns => 'RdRefinedMetalGrade') {
      parameter 'entity', :RdRefinedMetalGrade
    }

  }

  # Shape Service
  service('Shape Service') {

    operation('Get List', :returns => list('RdRefinedMetalShape')) {
    }

    operation('Get Versions', :returns => list('RdRefinedMetalShape')) {
        parameter 'guid', :string
    }

    operation('Get By GUID', :returns =>  'RdRefinedMetalShape') {
      parameter 'guid', :string
    }

    operation('Cancel Version') {
        parameter 'guid',    :string
        parameter 'ver',   :integer
    }

    operation('Save', :returns => 'RdRefinedMetalShape') {
        parameter 'entity', :'RdRefinedMetalShape'
    }

  }

  # Brand Service
  service('Brand Service') {

    operation('Get List', :returns => list('RdRefinedMetalBrand')) {
    }

    operation('Get Versions', :returns => list('RdRefinedMetalBrand')) {
        parameter 'guid', :string
    }

    operation('Get By GUID', :returns =>  'RdRefinedMetalBrand') {
      parameter 'guid', :string
    }

    operation('Cancel Version') {
        parameter 'guid',    :string
        parameter 'ver',   :integer
    }

    operation('Save', :returns => 'RdRefinedMetalBrand') {
        parameter 'entity', :'RdRefinedMetalBrand'
    }


  }
}


