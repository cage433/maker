#version '0.2'

# This is the point of entry into the DSL. Classes are imported here, in turn these may import other classes.

# model
import 'model-common.rb'
import 'model-lookups-private.rb'
import 'model-concentrations-private.rb'
import 'model-currencies-private.rb'
import 'model-countries-private.rb'
import 'model-locations-private.rb'
import 'model-commodities-private.rb'
import 'model-counterparties-private.rb'
import 'model-costcodes-private.rb'
import 'model-creditlimits-private.rb'

import 'dto-filtering-private.rb'

import 'model-departments-private.rb'
import 'model-users-private.rb'
import 'model-books-private.rb'
import 'model-roles-private.rb'
import 'model-permissions-private.rb'
import 'model-restrictions-private.rb'
import 'model-currencies-private.rb'
import 'model-ratings-private.rb'
import 'model-holidays-private.rb'

# services
import 'services-countries-lookups-private.rb'
import 'services-concentrations-private.rb'
import 'services-counterparties-private.rb'
import 'services-locations-private.rb'
import 'services-costcodes-private.rb'
import 'services-creditlimits-private.rb'

import 'services-departments-private.rb'
import 'services-users-private.rb'
import 'services-books-private.rb'
import 'services-roles-private.rb'
import 'services-permissions-private.rb'
import 'services-restrictions-private.rb'
import 'services-currencies-private.rb'
import 'services-ratings-private.rb'
import 'services-holidays-private.rb'
import 'services-commodities-private.rb'
