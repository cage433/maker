# EDM package EDM.Enterprise.Common Domains.Reference Data.Master Data.Legal Entities
in_namespace('commondomains.referencedata.masterdata.legalentities') {
=begin
  This entity maps to the CONTACTS table
=end
  define('Contact', :extends => 'Entity') {
    # This field maps to CO_NAME column
    field 'name',                     :string
    # This field maps to the PHONE column
    field 'phone',                    :string
=begin
    The fields below are in physical but not in the EDM
    # Maps via CONTACT_ADDRESSES.CO_ID = this.id & AD_ID = ADDRESSES.ID
    field 'addresses',                :list, :element_type => 'PostalAddress'
    # Maps via CODE_LOOKUPS.ID = this.STATUS_ID
    field 'status',                   'Status'
=end
  }

=begin	
  This entity maps to the LEGAL_ENTITIES table
=end
  define('Company', :extends => 'Entity') {
    # This field maps to the SHORT_NAME column
    field 'companyCode',              :string
    # This field maps to the LE_NAME column
    field 'name',                     :string
    # TODO: this field must be clarified with Nick
    # Mapping via the ADDRESSES.LE_ID = this.ID
    field 'addresses',                :list, :element_type => 'PostalAddress'
    # This field maps to the CODE_LOOKUPS table
    # where LEGAL_ENTITIES.ENTITY_ROLE = CODE_LOOKUPS.ID
    field 'roles',                    :list, :element_type => 'CompanyRole'
    # Mapping via COMMENTS.LE_ID
    field 'notes',                    :list, :element_type => 'Note'
    field 'entityRatings',            :list, :element_type => 'EntityRating'
    # This field maps to KYC_APPROVED column
    field 'kycApproved',              :boolean
    # This field maps to the CONCENTRATION_LIMITS table
    # where CONCENTRATION_LIMITS.LE_ID = LEGAL_ENTITIES.ID
    #   and CONCENTRATION_LIMITS.CONC_ID = CODE_LOOKUPS.ID
    field 'concentrationLevel',       'ConcentrationLevel'

    # Mapping via CONTACTS.LE_ID = LEGAL_ENTITIES.ID
    field 'contacts',                 :list, :element_type => 'commondomains.referencedata.masterdata.legalentities.Contact'
    # Mapping via BANK_ACCOUNTS.LE_ID
    field 'bankAccounts',             :list, :element_type => 'BankAccount'

=begin
    Mapping with the usage of INTERESTS.PARENT_ID where SUB_ID = this.ID
    and REL_TYPE points to CODE_LOOKUPS where CL_VALUE = PAR
=end
    field 'parentCompanies',          :list, :element_type => 'Shareholding'
=begin
    Mapping with the usage of INTERESTS.SUB_ID where PAR_ID = this.ID
    and REL_TYPE points to CODE_LOOKUPS where CL_VALUE = PAR
=end
    field 'childCompanies',           :list, :element_type => 'Shareholding'

=begin
    TODO: The fields below are only on the Physical
    # Mapping via ACCOUNT_CODES.LE_ID = this.ID
    field 'accCodes',                 :list, :element_type => 'Account'
    # Mapping via this.INC_COUNTRY_ID = COUNTRIES.ID
    field 'countryOfInc',             'Country'
    # Mapping via this.RISK_COUNTRY_ID = COUNTRIES.ID
    field 'riskCountry',              'Country'
=end
  }

=begin
  This maps to INTERESTS table and it's agnostic about the relation type
  The relation type can be parent or child and based on the appearance of
  the entity.

  INTERESTS.REL_TYPE points to CODE_LOOKUP
  SHORT_CODE :: RELT
  CL_VALUE   :: PAR, CHILD
  CL_DESC    :: Parent, Child
=end
  define('Shareholding') {
    field 'percentage',               :percent
    # It uses the PARENT_ID field
    field 'relatedCompany',           'Company'
  }

  # This table maps to LE_GROUP_COMPANIES table which is an extension of LEGAL_ENTITIES
  define('GroupCompany', :extends => 'Company') {
    field 'shortName',                :string
    field 'currency',                 'commondomains.referencedata.masterdata.unitsofmeasure.Currency'
    # TODO please see accounting package
    field 'glAccount',                'AccountType'
    # TODO seems missing from the physical
    #field 'riskRatePercent',          'Quantity'
    # TODO this field is missing from the physical
    #field 'riskCalculationPoint',     'RiskCalculationPoint'
    # Risk stream
    #field 'riskPremium',              'Quantity'
    # This field is a flag needs to mapped
    field 'eligibleForSecuritisation',:boolean
  }

=begin
  TODO: this is not active in EDM
  # Maps to LE_BANKS table which is an extension of LEGAL_ENTITIES
  define('Bank', :extends => 'Company') {
    # Please include the fields which are on the table exluding VERSIONING related fields
  }
=end

  define('Exchange', :extends => 'Company') {
    field 'commodities',             :list, :element_type => 'Commodity'
    field 'calendar',                'commondomains.referencedata.masterdata.calendars.HolidaysCalendar'
  }


  # Maps to BANK_ACCOUNTS table
  # TODO no fields in the EDM
  define('BankAccount', :extends => 'Entity') {
=begin
    This field maps to the CODE_LOOKUPS table
    where CODE_LOOKUPS.ID = BANK_ACCOUNTS.CCY_ID
=end
    field 'currency',                 'commondomains.referencedata.masterdata.unitsofmeasure.Currency'
    # This field maps to the BAC_NUMBER colun
    field 'accountNumber',            :string
=begin
    This field maps to the LEGAL_ENTITIES table
    where LEGAL_ENTITIES.ID = BANK_ACCOUNTS.LE_ID
=end
    field 'bank',                     'Company'
=begin
    This field maps to the STATUSES table
    where STATUSES.ID = BANK_ACCOUNTS.STATUS_ID
=end
    field 'status',                   'Status'

=begin
    # Maps via ACCO_ID + ACCO_VER
    field 'accCode',                  'AccountType'
=begin
    Please include the fields which are on the table exluding VERSIONING related fields
    this.COVER_TROUGH_ID = LEGAL_ENTITIES.ID

    field 'coveredCompany',           'Company'
=end
  }
}
