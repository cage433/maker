in_namespace('inner.ReferenceData.CreditLimit') {

  service('Credit Limit Type Service') {

    operation('Get List', :returns => list('RdCreditLimitType')) {
    }

    operation('Get Versions', :returns => list('RdCreditLimitType')) {
        parameter 'guid', :string
    }

    operation('Get By GUID', :returns =>  'RdCreditLimitType') {
      parameter 'guid', :string
    }

    operation('Cancel Version') {
        parameter 'guid',    :string
        parameter 'ver',   :integer
    }

    operation('Save', :returns => 'RdCreditLimitType') {
        parameter 'entity', :'RdCreditLimitType'
    }


  }

}