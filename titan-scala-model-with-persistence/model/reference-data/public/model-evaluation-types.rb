# EDM package EDM.Enterprise.Common Types.Evaluation Types
in_namespace('commontypes.evaluationtypes') {
  define('Number') {
    field 'numberType',                     'NumericType'
  }

  define('Integer', :extends => 'Number') {
    field 'amount',                   :integer
  }

  define('FixedDecimal', :extends => 'Number') {
    field 'amount',                   :real
    field 'decimalPlaces',            :integer
  }

  define('FloatingPoint', :extends => 'Number') {
    field 'amount',                   :real
  }

  define('Quantity', :extends => 'Entity') {
    #field 'uom',            'UnitOfMeasure'
  }

  define('Tolerance') {
    field 'plus',                     'commontypes.evaluationtypes.Quantity'
    field 'minus',                    'commontypes.evaluationtypes.Quantity'
  }
}
