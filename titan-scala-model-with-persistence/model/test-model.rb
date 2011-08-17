# Test service, purely for unit/component testing
# of the service framework, services, serialisation,
# binding-generator and potentially db persistence layer too

# this is a test model to model elements, data types and services
in_namespace('TradeCapture.Internal.TestModel') {

  # test model entity, for testing generated types and code
  define('TestModelDSLTypes') {
    field 'monthyear',          :monthYear
    field 'datetime',           :datetime
    field 'someDummyField',     :string
  }

  define('TestModelDSLReadOnlyType') {
    field 'field1',             :integer
    field 'field2',             :string
  }
}
