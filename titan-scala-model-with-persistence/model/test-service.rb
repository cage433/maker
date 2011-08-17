# Test service, purely for unit/component testing of the service framework, services, serialisation, binding-generator and potentially db persistence layer too

in_namespace('TradeCapture.Internal.TestService') {

  service('TestService') {

    operation('CreateTestEntity', :returns => :integer) {
      parameter 'testEntity', :TestModelDSLTypes
    }

    operation('GetTestEntity', :returns => :TestModelDSLTypes) {
     parameter 'oid', :integer
    }

    operation('UpdateTestEntity') {
      parameter 'testEntity', :TestModelDSLTypes
    }

    operation('LoopbackTestEntity', :returns => :TestModelDSLTypes) {
      parameter 'testEntity', :TestModelDSLTypes
    }

    operation('GetTestReadOnlyEntity', :returns => :TestModelDSLReadOnlyType) {
      parameter 'oid', :integer
    }
  }
}
