import 'averaging-leg-model.rb'

in_namespace('Quantity Schedules') {
  
  # This defines each a single leg of a set of rules that make up a quantity schedule.
  define('Quantity Schedule Rule') {
    field 'quantity',               'Quantity'
    field 'conversion factor',      :real,      :optional => true
    field 'proportion or fixed',    :enum,      :enumerated => ['Proportional', 'Fixed']
    field 'mtm leg',                'Averaging Leg'
  }

  # A quantity schedule is a set of rules designed to mark up the total volume of a strategy.
  define('Quantity Schedule') {
    field 'mass or volume driven',  :enum,      :enumerated => ['Mass', 'Volume']
    field 'is stock in tank',       :boolean
  
    # This is an ordered list of the rules to be applied to the total quantity of all trades
    # within a strategy.
    field 'rules',                  :list,      :element_type => 'Quantity Schedule Rule'
  }
}
