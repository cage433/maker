in_namespace('TradeCapture.Internal.TestModel', :hibernate_persist => false) {
  define('TestModelDSLTypes', :persistence_name => "TestModelDSLTypesTbl") {
    field 'monthyear', :persistence_name => "monthyear_field1" #, :hibernate_column_name => "test_hibernate_field_name"
    field 'datetime', :persistence_name => "monthyear_field2"
  }

  # Will not work until S-03115 (support for persistence control) is merged
  #define('TestModelDSLReadOnlyType', :persistence_name => "TestModelDSLReadOnlyTypeTbl", :scala_read_only => true)
}
