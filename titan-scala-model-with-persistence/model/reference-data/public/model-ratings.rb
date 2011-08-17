# EDM package EDM.Enterprise.Common Domains.Reference Data.Master Data.Ratings
in_namespace('commondomains.referencedata.masterdata.ratings') {

  define('CreditRating', :extends => 'EnumeratedItem') {
  }

  define('CreditRatingType', :extends => 'EnumeratedItem') {
  }

  define('CreditRatingGroupCode', :extends => 'EnumeratedItem') {
  }

  define('WatchlistReason', :extends => 'EnumeratedItem') {
  }

  # HOWTO: fill all this lists
  define('RatingAgency',      :extends => 'EnumeratedItem') {
    field 'creditRatings',          :list, :element_type => 'CreditRating'
    field 'creditRatingTypes',      :list, :element_type => 'CreditRatingType'
    field 'creditRatingGroupCodes', :list, :element_type => 'CreditRatingGroupCode'
    field 'entityRatings',    :list, :element_type => 'EntityRating'
    field 'watchlistReasons', :list, :element_type => 'WatchlistReason'
  }

  define('EntityRating',   :extends => 'Entity') {
    field 'ratingAgency',  'RatingAgency'
  }

  define('TrafiguraEntityRating', :extends => 'EntityRating') {
    field 'rating',               'CreditRating'
    # TODO: change to 'commondomains.referencedata.masterdata.security.User'
    # when generator will be fixed to support crossrefferences between files
    field 'recordedBy',           :string
    field 'effectiveDateRange',   'commontypes.dataspecifications.DateRange'
  }

  define('ExternalRating',    :extends => 'EntityRating') {
  }

  define('MoodysOutlook',  :extends => 'Entity') {
    field 'outlook',       :string
    field 'outlookDate',   :datetime
    field 'effectiveDateRange',   'commontypes.dataspecifications.DateRange'
  }

  define('MoodysWatchlistIndicator', :extends => 'Entity') {
    field 'watchlistIndicator',      :string
    field 'watchlistDate',           :datetime
    field 'watchlistReason',         'WatchlistReason'
    field 'effectiveDateRange',      'commontypes.dataspecifications.DateRange'
  }

  define('MoodysSpecificRating',              :extends => 'Entity') {
    field 'ticker',                           :string
    field 'overallRating',                    :string
    field 'overallRatingDate',                :datetime
    field 'longTermRating',                   'CreditRating'
    field 'longTermRatingDate',               :datetime
    field 'longTermRatingClass',              :string
    field 'issuerRating',                     'CreditRating'
    field 'issuerRatingDate',                 :datetime
    field 'issuerRatingForeignCurrency',      'CreditRating'
    field 'issuerRatingForeignCurrencyDate',  :datetime
    field 'issuerRatingDomesticCurrency',     'CreditRating'
    field 'issuerRatingDomesticCurrencyDate', :datetime
    field 'shortTermMostRecentRating',        'CreditRating'
    field 'shortTermMostRecentRatingDate',    :datetime
    field 'shortTermMostRecentRatingClass',   :string
    field 'corporateFamilyRating',            'CreditRating'
    field 'corporateFamilyRatingDate',        :datetime
    field 'estimatedSeniorRating',            'CreditRating'
    field 'estimatedSeniorRatingDate',        :datetime
    field 'effectiveDateRange',               'commontypes.dataspecifications.DateRange'
  }

  define('MoodyRating',                       :extends => 'ExternalRating') {
    field 'moodysSpecificRatings',            :list, :element_type => 'MoodysSpecificRating'
    field 'moodysOutlooks',                   :list, :element_type => 'MoodysOutlook'
    field 'moodysWatchlistIndicators',        :list, :element_type => 'MoodysWatchlistIndicator'
  }

  define('SPRatingSubsector',  :extends => 'Entity') {
    field 'subsector',         :string
    field 'effectiveDateRange',   'commontypes.dataspecifications.DateRange'
  }

  define('SPRatingSector',   :extends => 'Entity') {
    field 'sector',          :string
    field 'subsectors',      :list, :element_type => 'SPRatingSubsector'
    field 'effectiveDateRange',   'commontypes.dataspecifications.DateRange'
  }

  define('SPRatingEndorsementIndicator', :extends => 'Entity') {
    field 'endorsementIndicator',         :string
    field 'effectiveDateRange',   'commontypes.dataspecifications.DateRange'
  }

  define('SPRatingLocation', :extends => 'Entity') {
    field 'countryCode',      :string
    field 'stateCode',        :string
    field 'effectiveDateRange',   'commontypes.dataspecifications.DateRange'
  }

  define('SPRatingRegion', :extends => 'Entity') {
    field 'region',         :string
    field 'effectiveDateRange',   'commontypes.dataspecifications.DateRange'
  }

  define('SPRatingFlag',      :extends => 'Entity') {
    field 'unsolicitedRating', :boolean
    field 'regulatoryRating',  :boolean
    field 'effectiveDateRange',   'commontypes.dataspecifications.DateRange'
  }

  define('SPRatingIdentification', :extends => 'Entity') {
    field 'legalName',              :string
    field 'shortName',              :string
    field 'primarySIC',             :string
    field 'primaryGICS',            :string
    field 'primaryNAICS',           :string
    field 'primaryInsuranceNAIC',   :string
    field 'TSEC',                   :string
    field 'CUSIP',                  :string
    field 'CINS',                   :string
    field 'ticker',                 :string
    field 'effectiveDateRange',   'commontypes.dataspecifications.DateRange'
  }

  define('SPSpecificRating',         :extends => 'Entity') {
    field 'groupCode',                'CreditRatingGroupCode'
    field 'ratingType',               'CreditRatingType'
    field 'overallRating',            :string
    field 'overallRatingDate',        :datetime
    field 'longTermCreditwatch',      :string
    field 'longTermCreditwatchDate',  :datetime
    field 'shortTermCreditwatch',     :string
    field 'shortTermCreditwatchDate', :datetime
    field 'longTermOutlook',          :string
    field 'longTermOutlookDate',      :datetime
    field 'shortTermOutlook',         :string
    field 'shortTermOutlookDate',     :datetime
    field 'longTermRating',           'CreditRating'
    field 'longTermRatingDate',       :datetime
    field 'shortTermRating',          'CreditRating'
    field 'shortTermRatingDate',      :datetime
    field 'effectiveDateRange',   'commontypes.dataspecifications.DateRange'
  }

  define('SPRatingDetail',               :extends => 'ExternalRating') {
    field 'ratingSectors',               :list, :element_type => 'SPRatingSector'
    field 'ratingEndorsementIndicators', :list, :element_type => 'SPRatingEndorsementIndicator'
    field 'ratingLocations',             :list, :element_type => 'SPRatingLocation'
    field 'ratingRegions',               :list, :element_type => 'SPRatingRegion'
    field 'ratingFlags',                 :list, :element_type => 'SPRatingFlag'
    field 'ratingIdentifications',       :list, :element_type => 'SPRatingIdentification'
    field 'specificRatings',             :list, :element_type => 'SPSpecificRating'
  }

}
