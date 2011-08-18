in_namespace('inner.ReferenceData.CostCodes') {

  service('Cost Code Service') {

    operation('Get List', :returns => list('RdCostCode')) {
    }

    operation('Get Versions', :returns => list('RdCostCode')) {
        parameter 'guid', :string
    }

    operation('Get By GUID', :returns =>  'RdCostCode') {
      parameter 'guid', :string
    }

    operation('Cancel Version') {
        parameter 'guid',    :string
        parameter 'ver',   :integer
    }

    operation('Save', :returns => 'RdCostCode') {
        parameter 'entity', :'RdCostCode'
    }

  }


  service('CostCode Granularity Service') {

    operation('Get List', :returns => list('RdCostCodeGranularity')) {
    }

    operation('Get Versions', :returns => list('RdCostCodeGranularity')) {
        parameter 'guid', :string
    }

    operation('Get By GUID', :returns =>  'RdCostCodeGranularity') {
      parameter 'guid', :string
    }

    operation('Cancel Version') {
        parameter 'guid',    :string
        parameter 'ver',   :integer
    }

    operation('Save', :returns => 'RdCostCodeGranularity') {
        parameter 'entity', :'RdCostCodeGranularity'
    }


  }

  service('Cost Code Type Service') {

    operation('Get List', :returns => list('RdCostCodeType')) {
    }

    operation('Get Versions', :returns => list('RdCostCodeType')) {
        parameter 'guid', :string
    }

    operation('Get By GUID', :returns =>  'RdCostCodeType') {
      parameter 'guid', :string
    }

    operation('Cancel Version') {
        parameter 'guid',    :string
        parameter 'ver',   :integer
    }

    operation('Save', :returns => 'RdCostCodeType') {
        parameter 'entity', :'RdCostCodeType'
    }

  }   


}