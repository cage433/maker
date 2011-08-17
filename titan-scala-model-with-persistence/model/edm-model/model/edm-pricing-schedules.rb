in_namespace('EDM.PricingSchedules') {

  define('PricingSchedule', :abstract => true) {
    field 'oid',                           :integer, :identifier => true
  }

  define('OptionalPricingSchedule', :extends => 'PricingSchedule') {
  }

  define('NonOptionalPricingSchedule', :extends => 'PricingSchedule') {
  }

  define('FixedDatesPricingSchedule', :extends => 'NonOptionalPricingSchedule') {
    field 'dates',                         'DateRange'
  }
}
