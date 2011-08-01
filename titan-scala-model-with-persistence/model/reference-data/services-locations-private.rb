in_namespace('inner.ReferenceData.Locations') {

  service('Locations Service') {

    operation('Get Items Count', :returns => :integer) {
        parameter 'filterData',  'UNLocationsFilter'
    }

    operation('Get Items', :returns => list('RdUNLocation')) {
        parameter 'filterData',  'UNLocationsFilter'
    }

  }

}
