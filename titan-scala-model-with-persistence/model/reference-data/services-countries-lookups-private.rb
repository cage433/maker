in_namespace('inner.ReferenceData') {


  service('Business Line Service') {
    operation('Get List', :returns => list('RdBusinessLine')) {}

  }

  service('Lookup Type Service') {
    operation('Get List', :returns => list('RdLookupType')) {
    }
  }


  service('Languages Service') {
    operation('Get List', :returns => list('RdLanguage')) {}

  }

  service('Status Service') {
    operation('Get List', :returns => list('RdStatus')) {}

  }



  service('Lookups Service') {
    operation('Get List', :returns => list('RdLookup')) {
      parameter 'typeShortCode', :string
    }


    operation('Get Versions', :returns => list('RdLookup')) {
      parameter 'typeShortCode', :string
      parameter 'guid', :string
    }


    operation('Get By GUID', :returns => 'RdLookup') {
      parameter 'typeShortCode', :string
      parameter 'guid', :string
    }


    operation('Get By Short Code', :returns => 'RdLookup') {
      parameter 'typeShortCode', :string
      parameter 'shortCode', :string
    }


    operation('Cancel Version') {
      parameter 'guid', :string
      parameter 'ver', :integer
    }


    operation('Save', :returns => 'RdLookup') {
      parameter 'typeShortCode', :string
      parameter 'entity', :RdLookup
    }

  }


# Approved Statuses Service
  service('Approved Statuses Service') {
      #Returns all approved statuses without parameters
    operation('Get Full List', :returns => list('RdApprovedStatus')) {
    }
  }

# Political Unit Group Service
  service('Geograph Grouping Service') {
      #Returns all geographical groupings without parameters
    operation('Get Full List', :returns => list('RdPoliticalUnitGroup')) {
    }

    # Returns all the versions of a given Political Unit Group as it's stored in the databases. For versioning please see Reference Data SAD
    operation('Get Versions List', :returns => list('RdPoliticalUnitGroup')) {
      parameter 'id', :string
    }

    # Returns the currency by a shortCode. If the record can't be found with the provided key HTTP error code 500 is returned
    operation('Get By Short Code', :returns => 'RdPoliticalUnitGroup') {
      parameter 'shortCode', :string
    }

    # Cancel Version
    operation('Cancel Version') {
      parameter 'id', :string
      parameter 'ver', :integer
    }

=begin
    Persists the Political Unit Group in the database. For error conditions please see Reference Data SAD.
    If the saving was successful the service initiates the following CRUD events:
    If the save was invoked for a new record an 'Insert' event is initiated
    If the save was invoked for an existing record an 'Update' event is initiated
    For event details please see Reference Data SAD
=end
    operation('Save', :returns => 'RdPoliticalUnitGroup') {
      parameter 'lookup', :RdPoliticalUnitGroup
    }

  }

# Continents Service
  service('Continents Service') {
      #Returns all continents without parameters
    operation('Get Full List', :returns => list('RdContinent')) {
    }

    # Returns all the versions of a given Continent as it's stored in the databases. For versioning please see Reference Data SAD
    operation('Get Versions List', :returns => list('RdContinent')) {
      parameter 'shortCode', :string
    }

    # Returns the currency by a shortCode. If the record can't be found with the provided key HTTP error code 500 is returned
    operation('Get By Short Code', :returns => 'RdContinent') {
      parameter 'shortCode', :string
    }

    # Cancel Version
    operation('Cancel Version') {
      parameter 'id', :string
      parameter 'ver', :integer
    }

=begin
    Persists the Continent in the database. For error conditions please see Reference Data SAD.
    If the saving was successful the service initiates the following CRUD events:
    If the save was invoked for a new record an 'Insert' event is initiated
    If the save was invoked for an existing record an 'Update' event is initiated
    For event details please see Reference Data SAD
=end
    operation('Save', :returns => 'RdContinent') {
      parameter 'lookup', :RdContinent
    }

  }

# Regions Service
  service('Regions Service') {
      #Returns all regions without patameters
    operation('Get Full List', :returns => list('RdRegion')) {
    }

    # Returns all the versions of a given Region as it's stored in the databases. For versioning please see Reference Data SAD
    operation('Get Versions List', :returns => list('RdRegion')) {
      parameter 'id', :string
    }

    # Returns the currency by a shortCode. If the record can't be found with the provided key HTTP error code 500 is returned
    operation('Get By Short Code', :returns => 'RdRegion') {
      parameter 'shortCode', :string
    }

    # Cancel Version
    operation('Cancel Version') {
      parameter 'id', :string
      parameter 'ver', :integer
    }

=begin
    Persists the Region in the database. For error conditions please see Reference Data SAD.
    If the saving was successful the service initiates the following CRUD events:
    If the save was invoked for a new record an 'Insert' event is initiated
    If the save was invoked for an existing record an 'Update' event is initiated
    For event details please see Reference Data SAD
=end
    operation('Save', :returns => 'RdRegion') {
      parameter 'lookup', :RdRegion
    }

  }
# International Organisations Service
  service('International Organisations Service') {
      #Returns all international organisations without parameters
    operation('Get Full List', :returns => list('RdInternationalOrg')) {
    }

    # Returns all the versions of a given International Organisation as it's stored in the databases. For versioning please see Reference Data SAD
    operation('Get Versions List', :returns => list('RdInternationalOrg')) {
      parameter 'id', :string
    }

    # Returns the currency by a shortCode. If the record can't be found with the provided key HTTP error code 500 is returned
    operation('Get By Short Code', :returns => 'RdInternationalOrg') {
      parameter 'shortCode', :string
    }

    # Cancel Version
    operation('Cancel Version') {
      parameter 'id', :string
      parameter 'ver', :integer
    }

=begin
    Persists the International Organisation in the database. For error conditions please see Reference Data SAD.
    If the saving was successful the service initiates the following CRUD events:
    If the save was invoked for a new record an 'Insert' event is initiated
    If the save was invoked for an existing record an 'Update' event is initiated
    For event details please see Reference Data SAD
=end
    operation('Save', :returns => 'RdInternationalOrg') {
      parameter 'lookup', :RdInternationalOrg
    }

  }
# Ratings Agents Service
  service('Ratings Agents Service') {
      #Returns all Rating Agents without parameters
    operation('Get Full List', :returns => list('RdRatingAgency')) {
    }

    # Returns all the versions of a given Rating Agency as it's stored in the databases. For versioning please see Reference Data SAD
    operation('Get Versions List', :returns => list('RdRatingAgency')) {
      parameter 'shortCode', :string
    }

    # Returns the currency by a shortCode. If the record can't be found with the provided key HTTP error code 500 is returned
    operation('Get By Short Code', :returns => 'RdRatingAgency') {
      parameter 'shortCode', :string
    }

    # Cancel Version
    operation('Cancel Version') {
      parameter 'id', :string
      parameter 'ver', :integer
    }

=begin
    Persists the Rating Agent in the database. For error conditions please see Reference Data SAD.
    If the saving was successful the service initiates the following CRUD events:
    If the save was invoked for a new record an 'Insert' event is initiated
    If the save was invoked for an existing record an 'Update' event is initiated
    For event details please see Reference Data SAD
=end
    operation('Save', :returns => 'RdRatingAgency') {
      parameter 'lookup', :RdRatingAgency
    }

  }

# VAT Code Service
  service('Vat Code Service') {
      #Returns all Vat codes without parameters
    operation('Get Full List', :returns => list('RdVatCode')) {
    }

    # Returns all the versions of a given VAT ID as it's stored in the databases. For versioning please see Reference Data SAD
    operation('Get Versions List', :returns => list('RdVatCode')) {
      parameter 'shortCode', :string
    }

    # Returns the currency by a shortCode. If the record can't be found with the provided key HTTP error code 500 is returned
    operation('Get By Short Code', :returns => 'RdVatCode') {
      parameter 'shortCode', :string
    }

    # Cancel Version
    operation('Cancel Version') {
      parameter 'id', :string
      parameter 'ver', :integer
    }

=begin
    Persists the VAT code in the database. For error conditions please see Reference Data SAD.
    If the saving was successful the service initiates the following CRUD events:
    If the save was invoked for a new record an 'Insert' event is initiated
    If the save was invoked for an existing record an 'Update' event is initiated
    For event details please see Reference Data SAD
=end
    operation('Save', :returns => 'RdVatCode') {
      parameter 'lookup', :RdVatCode
    }

  }

# Transport Types Service
  service('Transport Types Service') {
      #Returns all transport types
    operation('Get Full List', :returns => list('RdTransportType')) {
    }

    # Returns all the versions of a given Transport Type
    operation('Get Versions List', :returns => list('RdTransportType')) {
      parameter 'id', :string
    }

    # Returns Transport type by short code
    operation('Get By Short Code', :returns => 'RdTransportType') {
      parameter 'shortCode', :string
    }

    # Cancel Version
    operation('Cancel Version') {
      parameter 'id', :string
      parameter 'ver', :integer
    }

    operation('Save', :returns => 'RdTransportType') {
      parameter 'lookup', :RdTransportType
    }

  }

# Order Request Types Service
  service('Order Request Types Service') {
      #Returns all transport types
    operation('Get Full List', :returns => list('RdOrderRequestType')) {
    }

    # Returns all the versions of a given Order Request Type
    operation('Get Versions List', :returns => list('RdOrderRequestType')) {
      parameter 'id', :string
    }

    # Returns Order Request type by short code
    operation('Get By Short Code', :returns => 'RdOrderRequestType') {
      parameter 'shortCode', :string
    }

    # Cancel Version
    operation('Cancel Version') {
      parameter 'id', :string
      parameter 'ver', :integer
    }

    operation('Save', :returns => 'RdOrderRequestType') {
      parameter 'lookup', :RdOrderRequestType
    }

  }


# Order Valid Time Service
  service('Order Valid Time Service') {
      #Returns all order valid times
    operation('Get Full List', :returns => list('RdOrderValidTime')) {
    }

    # Returns all the versions of a given Order Valid Time
    operation('Get Versions List', :returns => list('RdOrderValidTime')) {
      parameter 'id', :string
    }

    # Returns Order Valid Time by short code
    operation('Get By Short Code', :returns => 'RdOrderValidTime') {
      parameter 'shortCode', :string
    }

    # Cancel Version
    operation('Cancel Version') {
      parameter 'id', :string
      parameter 'ver', :integer
    }

    operation('Save', :returns => 'RdOrderValidTime') {
      parameter 'lookup', :RdOrderValidTime
    }

  }


# Order Basis Service
  service('Order Basis Service') {
      #Returns all Order Basis
    operation('Get Full List', :returns => list('RdOrderBasis')) {
    }

    # Returns all the versions of a given Order Basis
    operation('Get Versions List', :returns => list('RdOrderBasis')) {
      parameter 'id', :string
    }

    # Returns Order Basis by short code
    operation('Get By Short Code', :returns => 'RdOrderBasis') {
      parameter 'shortCode', :string
    }

    # Cancel Version
    operation('Cancel Version') {
      parameter 'id', :string
      parameter 'ver', :integer
    }

    operation('Save', :returns => 'RdOrderBasis') {
      parameter 'lookup', :RdOrderBasis
    }

  }


# Option Types Service
  service('Order Option Types Service') {
      #Returns all option types
    operation('Get Full List', :returns => list('RdOrderOptionType')) {
    }

    # Returns all the versions of a given OrderOptionType
    operation('Get Versions List', :returns => list('RdOrderOptionType')) {
      parameter 'id', :string
    }

    # Returns Option type by short code
    operation('Get By Short Code', :returns => 'RdOrderOptionType') {
      parameter 'shortCode', :string
    }

    # Cancel Version
    operation('Cancel Version') {
      parameter 'id', :string
      parameter 'ver', :integer
    }

    operation('Save', :returns => 'RdOrderOptionType') {
      parameter 'lookup', :RdOrderOptionType
    }

  }


# Option Contract Types Service
  service('Order Option Contract Types Service') {
      #Returns all option contract types
    operation('Get Full List', :returns => list('RdOrderOptionContractType')) {
    }

    # Returns all the versions of a given Option Contract Type
    operation('Get Versions List', :returns => list('RdOrderOptionContractType')) {
      parameter 'id', :string
    }

    # Returns Option Contract type by short code
    operation('Get By Short Code', :returns => 'RdOrderOptionContractType') {
      parameter 'shortCode', :string
    }

    # Cancel Version
    operation('Cancel Version') {
      parameter 'id', :string
      parameter 'ver', :integer
    }

    operation('Save', :returns => 'RdOrderOptionContractType') {
      parameter 'lookup', :RdOrderOptionContractType
    }

  }                                                                                                        
                                                            

  service('Country Service') {

    operation('Get List', :returns => list('RdCountry')) {
    }

    operation('Get Versions', :returns => list('RdCountry')) {
      parameter 'guid', :string
    }

    operation('Get By GUID', :returns => 'RdCountry') {
      parameter 'guid', :string
    }

    operation('Cancel Version') {
      parameter 'guid', :string
      parameter 'ver', :integer
    }

    operation('Save', :returns => 'RdCountry') {
      parameter 'entity', :RdCountry
    }

  }
}

in_namespace('inner.ReferenceData.Accounting') {

  service('Account Code Type Service') {
      #Returns all account code types
    operation('Get Full List', :returns => list('RdAccountCodeType')) {
    }
  }

}

in_namespace('inner.ReferenceData.LegalEntities') {

    # Returns List of Interest Types
  service('Interest Types Service') {
      # Returns all Interest Types
    operation('Get Full List', :returns => list('RdInterestType')) {}

  }

  # Returns List of Risk Offices
  service('Risk Office Service') {
      # Returns all Interest Types
    operation('Get Full List', :returns => list('RdRiskOffice')) {}

  }

  # Returns List of all Note Categories
  service('Note Categories Service') {
      # Returns all Default Bank Charges.
    operation('Get Full List', :returns => list('RdCategory')) {}

  }

  # Returns List of Contact Roles
  service('Contact Roles Service') {
      # Returns all Interest Types
    operation('Get Full List', :returns => list('RdContactRole')) {}

  }

  # Returns List of all Default Bank Charges
  service('Default Bank Charges Service') {
      # Returns all Default Bank Charges.
    operation('Get Full List', :returns => list('RdDefBankCharge')) {}

  }

  service('Counterparty Flag Service') {
      #Returns all flags
    operation('Get Full List', :returns => list('RdFlag')) {
    }
  }


  service('Registred Office Service') {
      #Returns all flags
    operation('Get Full List', :returns => list('RdRegistredOffice')) {
    }
  }

  service('Month Service') {
      #Returns all flags
    operation('Get Full List', :returns => list('RdMonth')) {
    }
  }

  service('Bank Account Flag Service') {
      #Returns all flags
    operation('Get Full List', :returns => list('RdBankAccountFlag')) {
    }
  }

  service('Header Footer Service') {
      #Returns all flags
    operation('Get Full List', :returns => list('RdHeaderFooter')) {
    }
  }

  service('Font Service') {
      #Returns all flags
    operation('Get Full List', :returns => list('RdFont')) {
    }
  }

  service('Image Position Service') {
      #Returns all flags
    operation('Get Full List', :returns => list('RdImagePosition')) {
    }
  }

  service('Vat Invoice Service') {
      #Returns all flags
    operation('Get Full List', :returns => list('RdVatInvoice')) {
    }
  }

  service('Secured Service') {
      #Returns all flags
    operation('Get Full List', :returns => list('RdSecured')) {
    }
  }

}

in_namespace('inner.ReferenceData.Locations') {

  service('Address Type Service') {

      #Returns all address types
    operation('Get Full List', :returns => list('RdAddressType')) {
    }
  }

  service('Location Type Service') {

      #Returns all address types
    operation('Get Full List', :returns => list('RdLogisticsLocationType')) {
    }
  }

}

in_namespace('inner.ReferenceData.CostCodes') {

  service('CostCode Direction Service') {

      #Returns all cost codes directions
    operation('Get Full List', :returns => list('RdCostCodeDirection')) {
    }
  }

  service('CostCode Flag Service') {

      #Returns all cost code flags
    operation('Get Full List', :returns => list('RdCostCodeFlag')) {
    }
  }

}

in_namespace('inner.ReferenceData.CreditLimit') {

  service('Credit Limit Category Service') {

      #Returns all credit limit categories
    operation('Get Full List', :returns => list('RdCreditLimitCategory')) {
    }
  }

  service('Credit Limit Behaviour Service') {

      #Returns all credit limit behaviours
    operation('Get Full List', :returns => list('RdCreditLimitBehaviour')) {
    }
  }

                                        
}





