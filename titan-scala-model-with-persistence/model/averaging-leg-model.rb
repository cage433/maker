import 'pricing-schedule-model.rb'

in_namespace('Averaging Legs') {
	# The averaging leg is a building block of commodity instruments pricing. We collect all the bits of information
	# we need in order to be able to produce a price. This requires us to know the following:
	# * which formula we need to evaluate
	# * which averaging period to average over (resolved from the pricing schedule)
	# * how to handle common vs non-common pricing when there are holiday conflicts
	# * whether to round each quote in a formula, and then sum (or whatever the coefficients are),
	# or evaluate the entire formula, and then round
	# * finally how many decimal places to round to when rounding
	# This should be sufficient to produce an actual price.
	define('Averaging Leg') {
		field 'formula',        'Formula'
		
		# TODO: ADD COMMENTS HERE!
		field 'basis formula',        'Formula'
		
		field 'pricing rule',   :enum,    :enumerated => ['Common', 'NonCommon'], :optional => true
		field 'pricing period', 'Pricing Schedule'
		field 'rounding style',     :enum,      :enumerated => ['Per Quote', 'Per Formula']
		field 'rounding',           :integer,  :optional => true
	}

	# This is an averaging leg where the formula contains a Worldscale freight quote for which the flat rate needs
	# resolving
	define('Freight Averaging Leg', :extends => 'Averaging Leg') {
		field 'flat rate year',     :integer,   :optional => true # If not present, price must be $/MT for wet freight
	}
}
