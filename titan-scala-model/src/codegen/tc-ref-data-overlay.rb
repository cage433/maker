in_namespace('TradeCapture.Internal.RefinedMetal') {

  # ref-data hibernate overlays...
  define('UOM', :hibernate_sequence_name => 'UOM_seq') {
    field 'guid', :hibernate_read_only_field => true
  }

  define('MarketLotSize', :hibernate_sequence_name => 'MarketLotSize_seq') {
    field 'guid', :hibernate_read_only_field => true
  }

  define('Counterparty', :hibernate_sequence_name => 'Counterparty_seq') {
    field 'guid', :hibernate_read_only_field => true
  }

  define('NonKYCCounterparty', :hibernate_sequence_name => 'Counterparty_seq') {
    field 'guid', :hibernate_read_only_field => true
  }
  
  define('ContractualTerms', :hibernate_sequence_name => 'ContractualTerms_seq') {
    field 'guid', :hibernate_read_only_field => true
  }

  define('Currency', :hibernate_sequence_name => 'Currency_seq') {
    field 'guid', :hibernate_read_only_field => true
  }

  define('DestinationLocation', :hibernate_sequence_name => 'DestinationLocation_seq') {
    field 'guid', :hibernate_read_only_field => true
  }

  define('Grade', :hibernate_sequence_name => 'Grade_seq') {
    field 'guid', :hibernate_read_only_field => true
  }

  define('GroupCompany', :hibernate_sequence_name => 'GroupCompany_seq') {
    field 'guid', :hibernate_read_only_field => true
  }

  define('Location', :hibernate_sequence_name => 'LocationX_seq') {
    field 'guid', :hibernate_read_only_field => true
  }

  define('Market', :hibernate_sequence_name => 'Market_seq') {
    field 'guid', :hibernate_read_only_field => true
  }

  define('Metal', :hibernate_sequence_name => 'Metal_seq') {
    field 'guid', :hibernate_read_only_field => true
    field 'neptuneMaterialCode', :hibernate_persist => false
  }

  define('PaymentTerms', :hibernate_sequence_name => 'PaymentTerms_seq') {
    field 'guid', :hibernate_read_only_field => true
  }

  define('PricingOffset', :hibernate_sequence_name => 'PricingOffset_seq') {
    field 'guid', :hibernate_read_only_field => true
  }

  define('SalesTradingRegion', :hibernate_sequence_name => 'SalesTradingRegion_seq') {
    field 'guid', :hibernate_read_only_field => true
  }

  define('SecurityTerms', :hibernate_sequence_name => 'SecurityTerms_seq') {
    field 'guid', :hibernate_read_only_field => true
  }

  define('Shape', :hibernate_sequence_name => 'Shape_seq') {
    field 'guid', :hibernate_read_only_field => true
  }

  define('TrafficHub', :hibernate_sequence_name => 'TrafficHub_seq') {
    field 'guid', :hibernate_read_only_field => true
  }

  define('QPType', :hibernate_sequence_name => 'QPType_seq') {
    field 'guid', :hibernate_read_only_field => true
  }

  define('Direction', :hibernate_sequence_name => 'Direction_seq') {
    field 'guid', :hibernate_read_only_field => true
  }
}
