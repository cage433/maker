# EDM common types

in_namespace('EDM') {
  in_namespace('shared.types') {

    define('UOM', :abstract => true) {
      field 'oid',                      :integer, :identifier => true
    }
    define('FundamentalUOM', :extends => 'EDM.shared.types.UOM') {
      field 'name', :string
    }
    define('Currency', :extends => 'FundamentalUOM') {
    }
    define('UnitComponent') {
      field 'oid',                      :integer, :identifier => true
      field 'exponent', :integer
      field 'fundamental', :'FundamentalUOM'
    }
    define('CompoundUOM', :extends => 'EDM.shared.types.UOM') {
      field 'components', :list, :element_type => 'UnitComponent'
    }

    # EDM_Quantity - a Price and Currency / Uom type representing some amount (possibly in a a currency) per UOM
    #   (in EDM currency is a UOM and quantity is a amount/uom type - and in the EDM the UOM is a recursively defined structure that can be composed of 1 or more other UOMs)
    #    refined-metals already has a simple UOM and currency type (defined by ref-data sources)
    #    - so further reconciliation will be needed here in the future)
    define('EDM_Quantity') {
      field 'amount',                   :real, :optional => true
      field 'currency',                 :integer_key, :optional => true, :references => 'TradeCapture.Internal.RefinedMetal.Currency(oid)' # 'Currency'
      field 'uom',                      :integer_key, :optional => true, :references => 'TradeCapture.Internal.RefinedMetal.UOM(oid)'
    }
    define('EQuantity') {
      field 'uom',                      :integer_key, :optional => true, :references => 'TradeCapture.Internal.RefinedMetal.UOM(oid)'
      field 'amount',                   :real, :optional => true
    }
    define('Quantity') {
      field 'amount',                   :real, :optional => true
      field 'uom',                      :'EDM.shared.types.CompoundUOM', :optional => true
    }

    # EDM has a more comprehensive date model, simple minimal daterange defined here initially
    define('DateSpec', :abstract => true) {
      field 'oid',                      :integer, :identifier => true
    }

    define('Date', :extends => 'DateSpec') {
      field 'datex',                    :date
    }

    define('DateRange') {
      field 'startDate',                :date
      field 'endDate',                  :date
    }


    define('Percentage') {
      field 'amount',                   :real, :optional => true
    }

    define('Tolerance') {
      field 'oid',                      :integer, :identifier => true
    }

    define('QuantityTolerance', :extends => 'Tolerance') {
      field 'plus',                     :'EQuantity'
      field 'minus',                    :'EQuantity'
    }

    define('PercentageTolerance', :extends => 'Tolerance') {
      field 'plus',                     :'Percentage'
      field 'minus',                    :'Percentage'
    }
  }
}
