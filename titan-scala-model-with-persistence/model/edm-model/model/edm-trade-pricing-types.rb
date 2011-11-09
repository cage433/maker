expose 'EDMHedgeRequest'
expose 'EDMPricingSpec'

# EDM model for initial FPC implementation

# Initial fragment of the EDM namespace for the Titan 1.3 FPC implementation
in_namespace('EDM.TradeMgmt.PhysicalTradeSpecs') {

  # Represents a single hedge request element
  define('EDMHedgeRequest', :abstract => true) {
    field 'oid',                            :integer, :identifier => true
    field 'ordinal',                  :integer
    field 'contractMonthName',        :string
    field 'hedgePrice',               'EDM.Common.Units.Quantity'
    field 'market',                   :guid
    field 'direction',                :string
  }

  # Represents a FX based hedge request for a specified amount
  define('EDMFxHedgeRequest', :extends => 'EDMHedgeRequest') {
    field 'amount',                   'EDM.Common.Units.Quantity'
  }

  # Represents a physical hedge request (in lots)
  define('EDMPhysicalHedgeRequest', :extends => 'EDMHedgeRequest') {
    field 'lots',                     :integer, :optional => true 
    field 'neptuneId',                :integer, :optional => true 
  }
  
  # Pricing specs with an FX component may use a rate other than that day's ECB fixing
  define('EDMFxOverride'){
    field 'oid',                            :integer, :identifier => true
    field 'date',                      :date
    field 'rate',                      'EDM.Common.Units.Quantity'
  }
  # A pricing specification determines a rule for calculating a price
  # in the context of some market data snapshot and a shipping month.
  # 
  # N.B. - This calculates a price, e.g. USD/MT - NOT a USD value.
  # The reason is that inventory items, quotas etc may all have the
  # same pricing specification - yet different prices and values.
  define('EDMPricingSpec', :abstract => true) {
    constant 'commentLength', 4000
    field 'oid',                      :integer, :identifier => true
    field 'ordinal',                  :integer
    field 'comments',                 :string, :optional => true, :max_length => EDMPricingSpec::COMMENTLENGTH
    field 'quantity',                 'EDM.Common.Units.Quantity'
    # If a pricing spec has a currency that is different to the associated market
    # then conversions will be done using ECB rate, unless there is an override.
    # Currency and fx overrides make no sense for anything other than unknown
    # and average pricing specs - however I'm not going to make the type hierarchy
    # any deeper
    field 'currency',                 :'EDM.Common.Units.Currency'
    field 'fxOverrides',              :list, :element_type => 'EDMFxOverride'
  }

  define('EDMFixedPrcComponent') {
    field 'oid',                            :integer, :identifier => true
   constant 'brandLength', 50
   # Do we need a date field - the data always appears to have the contract date here
   field 'date',                     :date, :optional => true
   field 'quantity',                 'EDM.Common.Units.Quantity'
   field 'price',                    'EDM.Common.Units.Quantity'
   field 'brand',                    :string, :max_length => EDMFixedPrcComponent::BRANDLENGTH # ultimately referring to some ref-data but for now this is unavailable so we're capturing free-form text
  }

  # A single quota may be broken into components, each of a different brand
  # As to why this is the only pricing spec with hedge requests... That is an open question
  define('EDMFixedPricingSpec', :extends => 'EDMPricingSpec') {
    field 'comps',               :list, :element_type => 'EDMFixedPrcComponent'
    field 'hedges',            :list, :element_type => 'EDMHedgeRequest'
    field 'premium',              'EDM.Common.Units.Quantity'
  }

  # Some trades have their price fixed by one party exercising their right
  # to fix a price using the 'observed' market price during the shipping
  # month (or some offset to it), with some premium added.
  # There will be one or more such exercises,until the entire quota is fixed
  # Note that these may be negative as 'unfixings' occur when the
  # counterparty uses this mechanism to punt.
  define('EDMUnkPricingFixation') {
    field 'oid',                            :integer, :identifier => true
    field 'fixedQuantity',            'EDM.Common.Units.Quantity'
    field 'observedPrice',        'EDM.Common.Units.Quantity'  # Note that this is the market price - the premium
  }

  define('EDMUnkPricingSpec', :extends => 'EDMPricingSpec') {
    field 'market',               :guid
    field 'qpMonth',              :date
    field 'fixations',            :list, :element_type => 'EDMUnkPricingFixation'
    # declarationBy might need to be a rule that maps a shipping month to a date
    field 'declarationBy',        :date
    field 'optionality',          :enum, :enumerated => ['Buyer', 'Seller', 'None'], :optional => true
    field 'premium',              'EDM.Common.Units.Quantity'
  }
  
  # Remarkable similar to EDMUnkPricingFixation, however that is trade data. The
  # below is market data and should eventually stop being stored with the trade
  define('EDMAvePriceFixation') {
    field 'oid',                            :integer, :identifier => true
    field 'oneDayFixingQuantity',           'EDM.Common.Units.Quantity'
    field 'price',           'EDM.Common.Units.Quantity'
  }

  define('EDMAvePricingSpec', :extends => 'EDMPricingSpec') {
    field 'fixations',          :list, :element_type => 'EDMAvePriceFixation'
  }  

  # This is the most common pricing specification. A typical
  # trade may be - 
  #   I buy 100 MT per month over 2010 at a price calculated as the
  #   average LME Zinc price during M + 1 plus a premium of 1.5 $/MT
  define('EDMMonthAveragePricingSpec', :extends => 'EDMAvePricingSpec') {
    field 'market',               :guid
    field 'qpMonth',              :date
    field 'premium',              'EDM.Common.Units.Quantity'
  }
  
  # Partial averages may have different quantities for each day of averaging
  define('EDMPartialAveDayQty'){
    field 'oid',                            :integer, :identifier => true
    field 'date',                 :date
    field 'quantity',             'EDM.Common.Units.Quantity'
  }

  # Similar to the more usual monthly average, however
  # averaging is done over some sub-period
  define('EDMPartAvePrcSpec', :extends => 'EDMAvePricingSpec'){
    field 'market',               :guid
    field 'premium',              'EDM.Common.Units.Quantity'
    # Note that the sum of quantities in this map should equal the pricing spec's own quantity
    field 'dayQtyMap',       :list, :element_type => 'EDMPartialAveDayQty'
  }

  # A pricing specification formed from the weighted average of other pricing
  # specifications. 
  define('EDMPriceSpecificationWeighting'){
    field 'oid',                            :integer, :identifier => true
    field 'weight',               :real
    field 'pricingSpec',          'EDMPricingSpec'
  }
  define('EDMWtdPricingSpec', :extends => 'EDMPricingSpec'){
    field 'wtdSpecs',         :list, :element_type => 'EDMPriceSpecificationWeighting'
  }

  # Represents the case where one party has the right to choose from a number
  # of different pricing specifications by some decision day.
  # Until the exercise is done pricing will use the first element in 'choices'. 
  define ('EDMOptPricingSpec'){
    field 'oid',                            :integer, :identifier => true
    field 'optionality',            :enum, :enumerated => ['Buyer', 'Seller', 'None'], :optional => true
    # declarationBy might need to be a rule that maps a shipping month to a date
    field 'declarationBy',        :date
    field 'choices',              :list, :element_type => 'EDMPricingSpec'
    # This field is populated once the choice has been made
    field 'chosenSpec',           'EDMPricingSpec', :optional => true
  }

}
