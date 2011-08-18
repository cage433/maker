in_namespace('inner.ReferenceData.LegalEntities') {

  define('RdLegalEntity') {
    constant 'roleBank', 'BK'
    constant 'roleCounterparty', 'CP'
    constant 'roleBroker', 'BR'
    constant 'roleGroupCompany', 'GC'
    constant 'roleShippingAgency', 'SHPAG'
    constant 'roleExchange', 'EXCHANGE'
    
    constant 'flagBanksGroup', 'BNKGR'

    constant 'nameFieldName', 'LE_NAME'
    
    field 'id',                         :string
    mixin('MainVersionable')

    field 'name',                       :string
    #Field added for generic translations in Public API
    field 'description',                :string
    field 'shortName',                  :string
    field 'website',                    :string
    field 'groupCompanyOverride',       :boolean
    field 'approvedKYC',                :boolean
    field 'approvedBy',                 :string
    field 'approvedDate',               :datetime, :optional => true

    field 'statusId',                   :string
    field 'riskCountryId',              :string
    field 'countryOfIncId',             :string
    field 'baseCurrencyId',             :string
    field 'defaultBankChargeId',        :string

    field 'translations',               :list, :element_type => 'RdTranslation'
    field 'accCodes',                   :list, :element_type => 'RdAccountCode'
    field 'bankAccounts',               :list, :element_type => 'RdBankAccount'
    field 'contacts',                   :list, :element_type => 'RdContact'
    field 'addresses',                  :list, :element_type => 'RdAddress'
    field 'parentCompanies',            :list, :element_type => 'RdInterest'
    field 'childCompanies',             :list, :element_type => 'RdInterest'

    # TODO Check after merge with Lviv
    field 'agencyRatingRef',           :list, :element_type => 'RdLegalEntityAgencyRatingRef'
    field 'concLimits',                :list, :element_type => 'RdLegalEntityConcLimit'

    field 'flagsIds',                  :list, :element_type => :string
    field 'rolesIds',                  :list, :element_type => :string
    field 'businessLinesIds',          :list, :element_type => :string
    field 'groupCompaniesIds',         :list, :element_type => :string                 

    field 'leBankRef',                 'RdLeBank'
    field 'leGroupCompanyRef',         'RdLeGroupCompany'
    field 'leExchangeRef',             'RdLeExchange'
  }

  define('RdLeBank') {
    field 'id',                        :string
    mixin('ChildVersionable')

    field 'swiftBic',                  :string
    field 'correspondentBic',          :string
    field 'sortCode',                  :string
    field 'trafiguraBank',             :boolean
    field 'confirming',                :boolean
    field 'advising',                  :boolean
    field 'riskCover',                 :boolean
    field 'financingBank',             :boolean
    field 'isBankGroup',               :boolean

    field 'bankGroupId',               :string

    field 'branches',                  :list, :element_type => 'RdInterest'
    field 'subsidiaries',              :list, :element_type => 'RdInterest'

    field 'flagsIds',                  :list, :element_type => :string

  }

  define('RdLeExchange') {
    constant 'classShortCode', 'EXCHG'

    field 'id',                        :string
    mixin('ChildVersionable')
    
    field 'settelmentDays',             :integer
    field 'workingDays',                :boolean
    field 'sensitive',                  :boolean

    field 'ccyId', 			:string

    field 'commoditiesIds',             :list, :element_type => :string
  }


  define('RdLeGroupCompany') {
    field 'id',                        :string
    mixin('ChildVersionable')

    field 'code',                      :string
    field 'forBehalfOf',               :string
    field 't1',                        :string
    field 't4',                        :string
    field 'emplioyingGC',              :boolean
    field 'eligibleForSec',            :boolean
    field 'incDocImage',               :boolean
    field 'account',                   :string
    field 'imagePath',                 :string

    field 'parentMgtEntityId',         :string
    # TODO clarify if this Ref necessary
    field 'parentMgtEntityRef',        'RdLegalEntity'

    field 'regOfficeId',               :string
    field 'finYearStartId',            :string
    field 'groupTypeId',               :string
    field 'docImagePositionId',        :string
    field 'hedgingBankId',             :string
    field 'hedgingBankRef',            'RdLegalEntity'

    field 'interCompanyAccounts',      :list, :element_type => 'RdInterCompanyAccount'
    field 'vatDetails',                :list, :element_type => 'RdVatDetail'
    field 'estCountries',              :list, :element_type => 'RdEstCountry'
    field 'taxCodes',                  :list, :element_type => 'RdTaxCode'
    field 'docs',                      :list, :element_type => 'RdLegalEntityDoc'
    field 'relGroupCompanies',         :list, :element_type => 'RdRelatedGroupCompany'
  }


  define('RdInterest') {
    field 'id',                              :string
    mixin('ChildVersionable')

    field 'holding',                         :percent, :optional => true 
    field 'ultimate',                        :boolean

    field 'relTypeId',                       :string
    field 'interestTypeId',                  :string
    field 'relatedCompanyId',                :string
    field 'relatedCompanyRef',               'RdLegalEntity'

  }


  define('RdInterCompanyAccount') {
    field 'id',                              :string
    mixin('ChildVersionable')

    field 'accountCodeValue',                :string

    field 'businessLineId',                  :string
    field 'childCompanyId',                  :string
    field 'accountCodeId',                   :string
    field 'statusId',                        :string
  }


  define('RdLegalEntityDoc') {
    field 'id',                              :string
    mixin('ChildVersionable')

    field 'headerFooterText',                :string
    field 'fontSize',                        :integer
    field 'fontBold',                        :boolean
    field 'fontItalic',                      :boolean
    field 'fontUnderline',                   :boolean

    field 'headerFooterId',                  :string
    field 'fontId',                          :string
  }


  define('RdNote') {
    field 'id',                        :string
    mixin('ChildVersionable')

    field 'comment',                   :string
    field 'recDate',                   :datetime
    field 'author',                    :string

    field 'parentId',                  :string
    field 'parentVer',                 :integer
    field 'categoryId',                :string


  }


  define('RdContact') {
    constant 'jobTitleFieldName', 'JOB_TITLE'
    constant 'nameFieldName', 'CO_NAME'

    field 'id',                        :string
    mixin('ChildVersionable')

    field 'name',                      :string
    # Created for generic translation for Public API
    field 'description',               :string
    field 'jobTitle',                  :string
    field 'phone',                     :string
    field 'fax',                       :string
    field 'telex',                     :string
    field 'email',                     :string
    field 'phoneMob',                  :string
    field 'phoneHome',                 :string
    field 'phoneDirect',               :string

    field 'roleId',                    :string
    field 'statusId',                  :string

    field 'addresses',                 :list, :element_type => 'RdAddress'
    field 'translations',              :list, :element_type => 'RdTranslation'
  }


  define('RdAccountCode') {
    field 'id',                              :string
    mixin('ChildVersionable')

    field 'value',                           :string

    field 'accTypeId',                       :string
  }


  define('RdBankAccountCode') {
    field 'id',                              :string
    mixin('ChildVersionable')

    field 'value',                           :string

    field 'accTypeId',                       :string
  }


  define('RdBankAccount') {
    field 'id',                            :string
    mixin('ChildVersionable')

    field 'name',                          :string
    field 'number',                        :string
    field 'openedDate',                    :date
    field 'maturityDate',                  :date
    field 'journalType',                   :boolean
    field 'mainTrading',                   :boolean
    field 'pettyCash',                     :boolean
    field 'dummy',                         :boolean
    field 'receiptsDisabled',              :boolean
    field 'iban',                          :string
    field 'remittanceText',                :string
    field 'accountOfficer',                :string
    field 'approvedBy',                    :string
    field 'approvedDate',                  :datetime, :optional => true

    field 'securedId',                     :string
    field 'brokerId',                      :string
    field 'bankId',                        :string
    field 'bankRef',                       'RdLegalEntity'
    field 'coveredBankId',                 :string
    field 'currencyId',                    :string
    field 'statusId',                      :string
    field 'approvedStatusId',              :string
    field 'correspondentBankId',           :string
    field 'correspondentBankRef',          'RdLegalEntity'

    field 'accounts',                      :list, :element_type => 'RdBankAccountCode'
    field 'flagsIds',                      :list, :element_type => :string
    field 'businessLinesIds',              :list, :element_type => :string
  }


   define('RdRelatedGroupCompany') {
    field 'id',                            :string

    field 'groupCompanyCode',              :string

    field 'departmentId',                  :string
    field 'businessLineId',                :string
    field 'groupCompanyId',                :string
    field 'statusId',                      :string
  }

   define('RdVatDetail') {
    field 'id',                            :string
    mixin('ChildVersionable')

    field 'vatNumber',                     :string

    field 'countryId',                     :string
    field 'vatId',                         :string
    field 'countryVatId',                  :string
    field 'statusId',                      :string
  }

   define('RdEstCountry') {
    field 'id',                            :string
    mixin('ChildVersionable')

    field 'dateFrom',                      :date, :optional => true
    field 'dateTo',                        :date, :optional => true

    field 'countryId',                     :string
    field 'statusId',                      :string
  }


  define('RdTaxCode') {
    field 'id',                            :string
    mixin('ChildVersionable')

    field 'code',                          :string
    field 'description',                   :string
    field 'invoiceHeading',                :string
    field 'reqWording',                    :string
    field 'purchaseSales',                 :string
    field 'taxCodeT1',                     :boolean
    field 'promptForAad',                  :boolean
    field 'hallmark',                      :string

    field 'countryId',                     :string
    field 'statusId',                      :string
  }
     
 # This table maps to the LE_RATING_AGENT_REFERENCES
  define('RdLegalEntityAgencyRatingRef', :extends => 'RdEntity') {
    mixin('ChildVersionable')
    field 'agentId',        :string
    field 'reference',   :string
    field 'leId',           :string
    field 'leVer',        :integer
  }
    
 # This table maps to the LE_RATINGS, AGENT_RATINGS tables
  define('RdLegalEntityConcLimit') {
    # Unique identifier
    field 'id',               :string
    mixin('ChildVersionable')
    field 'concLimitId',      :string
  }

}