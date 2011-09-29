expose 'PricingFixation'
expose 'HedgeRequest'
expose 'PricingSpec'

# EDM model for initial FPC implementation

# Initial fragment of the EDM namespace for the Titan 1.3 FPC implementation
in_namespace('TradeMgmt.Internal.RefinedMetalFPC') {

  # a futures product spec, ref-data source TBD, so month-year + free-form text for now until contract ref-data available
  define('ContractMonthName') {
    constant 'contractMonthSuffixLength', 50
    field 'month',                    :monthYear
    field 'suffix',                   :string, :optional => true, :max_length => ContractMonthName::CONTRACTMONTHSUFFIXLENGTH
  }

  # Represents a single hedge request element
  define('HedgeRequest', :abstract => true) {
    field 'oid',                      :integer, :identifier => true
    field 'ordinal',                  :integer
    field 'exchange',                 :integer_key, :references => 'Market(oid)'
    field 'contractMonthName',        'ContractMonthName'
    field 'hedgePrice',               'EDM_Quantity'
    field 'direction',                :integer_key, :references => 'Direction(oid)'
    field 'hedgeRequestId',           :integer, :optional => true
  }

  # Represents a FX based hedge request for a specified amount
  define('FxHedgeRequest', :extends => 'HedgeRequest') {
    field 'amount',                   'EDM_Quantity'
  }

  # Represents a physical hedge request (in lots)
  define('PhysicalHedgeRequest', :extends => 'HedgeRequest') {
    field 'lots',                     :integer, :optional => true # number of lots as entered/displayed in the UI, converted to an actual amount/uom via ref data for use elsewhere
    field 'amount',                   'TradeMgmt.Internal.RefinedMetal.Quantity', :optional => true # converted to/from lots by commodity and market using ref-data, use this value for actual calculations etc
  }

  # Represents a single pricing fixation element
  define('PricingFixation') {
    constant 'brandLength', 50
    field 'oid',                      :integer, :identifier => true
    field 'ordinal',                  :integer
    field 'date',                     :date, :optional => true
    field 'quantity',                 'TradeMgmt.Internal.RefinedMetal.Quantity'
    field 'price',                    'EDM_Quantity'
    field 'brand',                    :string, :max_length => PricingFixation::BRANDLENGTH # ultimately referring to some ref-data but for now this is unavailable so we're capturing free-form text
  }
  
  define('NeptunePricingFixation') {
    constant 'brandLength', 50
    field 'oid',                      :integer, :identifier => true
    field 'ordinal',                  :integer
    field 'date',                     :date, :optional => true
    field 'quantity',                 'TradeMgmt.Internal.RefinedMetal.Quantity'
    field 'price',                    'EDM_Quantity'
    field 'brand',                    :string, :max_length => PricingFixation::BRANDLENGTH # ultimately referring to some ref-data but for now this is unavailable so we're capturing free-form text
    field 'fxRate',                   :real
    field 'hedgeRequestNumber',       :integer, :optional => true
  }

  define('NeptuneHedgeRequest') {
    field 'oid',                      :integer, :identifier => true
    field 'ordinal',                  :integer
    field 'exchange',                 :integer_key, :references => 'Market(oid)'
    field 'contractMonthName',        'ContractMonthName'
    field 'hedgePrice',               'EDM_Quantity'
    field 'direction',                :integer_key, :references => 'Direction(oid)'
    field 'hedgeRequestId',           :integer, :optional => true
    field 'lots',                     :integer, :optional => true
    field 'amount',                   'TradeMgmt.Internal.RefinedMetal.Quantity', :optional => true
  }

  # A pricing specification element
  define('PricingSpec', :abstract => true) {
    constant 'commentLength', 4000
    constant 'premiumCommentLength',  4000
    field 'oid',                      :integer, :identifier => true
    field 'ordinal',                  :integer
    field 'quantity',                 'TradeMgmt.Internal.RefinedMetal.Quantity'
    field 'currency',                 :integer_key, :references => 'TradeMgmt.Internal.RefinedMetal.Currency(oid)'
    field 'comments',                 :string, :optional => true, :max_length => PricingSpec::COMMENTLENGTH
    field 'premiumComments',          :string, :optional => true, :max_length => PricingSpec::PREMIUMCOMMENTLENGTH
    field 'pricingFixations',         :list, :element_type => 'PricingFixation'
    field 'hedgeRequests',            :list, :element_type => 'HedgeRequest'
    field 'neptunePricingFixations',  :list, :element_type => 'NeptunePricingFixation'
    field 'neptuneHedgeRequests',     :list, :element_type => 'NeptuneHedgeRequest'
    field 'premium',                  'EDM_Quantity'
  }

  # unfortunately this does not work(!) since when embedded in quota the bidirectional=true on specs causes a run-time error with hibernate, to be resolved...
  #define('PricingSpecification') {
  #  field 'pType',                     :enum, :enumerated => ['Floating', 'Fixed'] # indicate type of pricing used
  #  field 'specs',                    :list, :element_type => 'PricingSpec'
  #}
  #


   # Fixed Pricing type of pricing specification
   define('FixedPricing', :extends => 'PricingSpec') {
   }

  define('MonthAveragePricingSpec', :extends => 'PricingSpec') {
    field 'market',               :integer_key, :references => 'Market(oid)'
    field 'qpMonth',              :date  
  }
  

  define('PartialAvePricingSpec', :extends => 'PricingSpec'){
    field 'market',               :integer_key, :references => 'Market(oid)'
    field 'firstAveragingDay',    :date
    field 'lastAveragingDay',     :date
  }

  define('UnkPricingSpec', :extends => 'PricingSpec') {
    field 'market',               :integer_key, :references => 'Market(oid)'
    field 'qpMonth',              :date    
  }
  
  
}
