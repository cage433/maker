#version '0.2'

# This is the point of entry into the DSL. Classes are imported here, in turn these may import other classes.

# Model definition

import 'model-common.rb'
import 'model-miscellaneus.rb'
import 'model-enumerated-types.rb'

# TODO: original type can't be generated because of bug
import 'model-accounting.rb'
import 'model-ratings.rb'

import 'model-calendars.rb'
# TODO: original type can't be generated because of bug
import 'model-units-of-measure.rb'
import 'model-evaluation-types.rb'
import 'model-miscellaneus.rb'

import 'model-locations.rb'
import 'model-legal-entities.rb'
import 'model-banking.rb'
import 'model-accounting.rb'
import 'model-security.rb'

# Service definition
import 'services-enumerated-types.rb'
import 'services-calendars.rb'
import 'services-units-of-measure.rb'
import 'services-legal-entities.rb'
import 'services-locations.rb'
import 'services-ratings.rb'
import 'services-accounting.rb'
import 'services-security.rb'
