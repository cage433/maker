in_namespace('inner.security.restrictions') {
=begin
  Permissions Service
=end
  service('Restrictions Service') {

    operation('Get List', :returns => list('RdRestriction')) {
      parameter 'statusCode',   :string
    }

    operation('Get Full List', :returns => list('RdRestriction')) {
    }

    operation('Get By Id', :returns => 'RdRestriction') {
      parameter 'keyId',        :string
    }

    operation('Save or Update', :returns => 'RdRestriction') {
      parameter 'restriction',  :RdRestriction
    }

    operation('Get History', :returns => list('RdRestriction')) {
      parameter 'keyId',        :string
    }

    operation('Get Restriction Gategories List', :returns => list('RdRestrictionCategory')) {}
  }

  service('Properties Service') {
    operation('Get Property Entities List', :returns => list('RdPropertyEntity')){}
    operation('Get Properties List', :returns => list('RdProperty')) {}
    operation('Get Values List For Property', :returns => list('RdLookup')){
      parameter 'codesetId',        :string
    }
  }
}