# this is the model as used for Release 1 of RMET
in_namespace('TradeCapture.Internal.RefinedMetal') {

  # A quantity is a number together with a unit of measurement. E.g. 10 MTS
  define('Quantity') {
    field 'amount',                 :real,        :optional => true
    field 'uom',                    :integer_key, :optional => true, :references => 'TradeCapture.Internal.RefinedMetal.UOM(oid)'
  }

  define ('Quality') {
    field 'grade',                  :integer_key, :optional => true, :references => 'Grade(oid)'
    field 'shape',                  :integer_key, :optional => true, :references => 'Shape(oid)'
  }
}
