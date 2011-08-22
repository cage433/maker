# this is the model for Reference Data Project
in_namespace('inner.ReferenceData.Locations') {


  define('RdLocationFuncType') {
    field 'id',                   :string

    field 'shortCode',            :string
    field 'description',          :string
  }


  define('RdUNSubDivision') {
    field 'id',                       :string
    mixin('ChildVersionable')

    field 'shortCode',                :string
    field 'description',              :string

    field 'countryRef',               'RdCountry'
  }

  define('RdUNLocation') {
    field 'id',                       :string

    field 'shortCode',                :string
    field 'description',              :string
    field 'stdDescription',           :string
    field 'recDate',                  :date
    field 'latitude',                 :real, :optional => true
    field 'latNS',                    :string
    field 'longitude',                :real, :optional => true
    field 'lonEW',                    :string

    field 'subdivisionId',            :string
    field 'subdivisionRef',           'RdUNSubDivision'
    field 'countryId',                :string

    field 'functions',                :list, :element_type => 'RdLocationFuncType'
  }


  define('RdAddress') {
    constant 'addrLine1FieldName', 'ADD_1'
    constant 'addrLine2FieldName', 'ADD_2'
    constant 'addrLine3FieldName', 'ADD_3'
    constant 'addrLine4FieldName', 'ADD_4'
    constant 'cityFieldName', 'CITY'
    constant 'zipCodeFieldName', 'ZIP'
    constant 'provinceFieldName', 'PROV'

    field 'id',                       :string
    mixin('ChildVersionable')

    field 'addrLine1',                :string
    field 'addrLine2',                :string
    field 'addrLine3',                :string
    field 'addrLine4',                :string
    field 'city',                     :string
    field 'zipCode',                  :string
    field 'province',                 :string

    field 'location',                 'RdUNLocation'
    field 'statusId',	              :string
    field 'addressTypeId',	      :string
    field 'countryId',                :string

    field 'translations',             :list, :element_type => 'RdTranslation'
  }

  define('RdLogisticsLocation') {
    constant 'descriptionFieldName', 'FAC_NAME'
    field 'id',                     :string
    mixin('MainVersionable')

    field 'shortCode',              :string
    field 'description',            :string

    field 'countryId',              :string
    field 'statusId',               :string    

    field 'translations',           :list, :element_type => 'RdTranslation'

    field 'facilityTypeIds',        :list, :element_type => :string
    
  }

}

