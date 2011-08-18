in_namespace('inner.security.ratings') {

  define('RdMoodysOutlook', :extends => 'RdLookup') {
    constant 'Type Short Code', "MDYOL"
    constant 'Negative with directional differences', "NEG(m"
    constant 'Positive with directional differences', "POS(m"
    constant 'Developing with directional differences', "DEV(m"
    constant 'Withdrawn', "RWR"
    constant 'Stable', "STA"
    constant 'Positive', "POS"
    constant 'Negative', "NEG"
    constant 'Developing', "DEV"
    constant 'No Outlook', "NOO"
    constant 'Rating Under Review', "RUR"
    constant 'Stable with directional differences', "STA(m"
  }

  define('RdRatingType', :extends => 'RdLookup') {
    constant 'Type Short Code', "RTGTY"
    constant 'S_P Long Term Issuer Credit Ratings', "SLTIR"
    constant 'S_P Short Term Issuer Credit Ratings ', "SSTIR"
    constant 'Moodys Long Term And Issuer Ratings', "MLTIR"
    constant 'Moodys Short Term Taxable Ratings', "MSTTR"
    constant 'Trafigura Risk Rating', "TRR"
    constant 'Moodys Short Term Municipal Ratings', "MSTMR"
    constant 'Moodys Short Term Variable Rate Municipal Ratings', "MSVRM"
    constant 'Speculative Grade Liquidity Ratings', "MSGLR"
  }

  define('RdMoodysRatingType', :extends => 'RdLookup') {
    constant 'Type Short Code', "MDYRT"
    constant 'Long Term Rating', "LTR"
    constant 'Issuer Rating', "IR"
    constant 'Issuer Rating Foreign Currency', "IRFC"
    constant 'Issuer Rating Domestic Currency', "IRDC"
    constant 'Short Term Most Recent Rating', "STMRR"
    constant 'Corporate Family Rating', "CFR"
    constant 'Estimated Senior Rating', "ESR"
  }

  define('RdSPRatingType', :extends => 'RdLookup') {
    constant 'Type Short Code', "SPRT"
  }

  define('RdSPRatingGroupCode', :extends => 'RdLookup') {
    constant 'Type Short Code', "SPRGC"
    constant 'Issuer Credit Rating', "ICR"
  }

  # Maps to AGENT_RATINGS  table
  define('RdAgentRating', :extends => 'RdEntity') {
      #Implied FK to CODE_LOOKUPS (with no VER)
    field 'agentId', :string
    field 'rating', :string
    field 'typeId', :string
    field 'rank', :string
    field 'concLevelIds', :list, :element_type => :string
  }

# Maps to MOODYS_COMBINED_RATING_VALUES  table
  define('RdMoodysCombinedRatingValue', :extends => 'RdEntity') {
    field 'overalRating', :string
    field 'effectiveFrom', :datetime
    field 'effectiveTo', :datetime
    # Maps to MOODYS_ISSUERS table
    field 'misId',                     :string
  }

# Maps to MOODYS_OUTLOOK_VALUES  table
  define('RdMoodysOutlookValue', :extends => 'RdEntity') {
      # English translation corresponding to the lookup
    field 'outlookvalueId', :string
    field 'outlookDate', :datetime
    field 'effectiveFrom', :datetime
    field 'effectiveTo', :datetime
    # Maps to MOODYS_ISSUERS table
    field 'misId', :string
  }

# Maps to MOODYS_WATCHLIST_VALUES  table
  define('RdMoodysWatchlistValue', :extends => 'RdEntity') {
    field 'indicatorValue', :string
    field 'indicatorReason', :string
    field 'indicatorDate', :datetime
    field 'effectiveFrom', :datetime
    field 'effectiveTo', :datetime
    # Maps to MOODYS_ISSUERS table
    field 'misId', :string
  }

# Maps to MOODYS_RATING_VALUES  table
  define('RdMoodysRatingValue', :extends => 'RdEntity') {
      # English translation corresponding to the lookup
    field 'typeId', :string
    # English translation corresponding to the lookup
    # Maps to AGENT_RATINGS table
    field 'agentRating', 'RdAgentRating'
    field 'date', :datetime
    field 'ratingClass', :string
    field 'value', :string
    field 'effectiveFrom', :datetime
    field 'effectiveTo', :datetime
    # Maps to MOODYS_ISSUERS table
    field 'misId', :string
  }

# Maps to MOODYS_ISSUERS  table
  define('RdMoodysIssuer', :extends => 'RdEntity') {
    field 'name', :string
    field 'ticker', :string
    field 'number', :string
    field 'lastUpdate', :datetime
    # Maps to STATUSES table
    field 'statusId', :string

    field 'moodysCombinedRatingValues', :list, :element_type => 'RdMoodysCombinedRatingValue'
    field 'moodysRatingValues', :list, :element_type => 'RdMoodysRatingValue'
    field 'moodysWatchlistValues', :list, :element_type => 'RdMoodysWatchlistValue'
    field 'moodysOutlookValues', :list, :element_type => 'RdMoodysOutlookValue'

  }

  # Maps to S_P_SECTOR_VALUES  table
  define('RdSPSectorValue', :extends => 'RdEntity') {
      #Maps to S_P_ENTITIES table
    field 'spEntityId', :string
    field 'entitySector', :string
    field 'entitySubSector', :string
    field 'effectiveFrom', :datetime
    field 'effectiveTo', :datetime
  }

  # Maps to S_P_REGION_VALUES  table
  define('RdSPRegionValue', :extends => 'RdEntity') {
      #Maps to S_P_ENTITIES table
    field 'spEntityId', :string
    field 'regioneCode', :string
    field 'effectiveFrom', :datetime
    field 'effectiveTo', :datetime
  }

  # Maps to S_P_LOCATION_VALUES  table
  define('RdSPLocationValue', :extends => 'RdEntity') {
      #Maps to S_P_ENTITIES table
    field 'spEntityId', :string
    field 'countryCode', :string
    field 'stateCode', :string
    field 'effectiveFrom', :datetime
    field 'effectiveTo', :datetime
  }

  # Maps to S_P_FLAG_VALUES  table
  define('RdSPFlagValue', :extends => 'RdEntity') {
      #Maps to S_P_ENTITIES table
    field 'spEntityId', :string
    field 'unsolicitedRating', :boolean
    field 'regulatoryRating', :boolean
    field 'effectiveFrom', :datetime
    field 'effectiveTo', :datetime
  }

  # Maps to S_P_ENDORSEMENT_VALUES  table
  define('RdSPEndorsementValue', :extends => 'RdEntity') {
      #Maps to S_P_ENTITIES table
    field 'spEntityId', :string
    field 'indicatorValue', :string
    field 'effectiveFrom', :datetime
    field 'effectiveTo', :datetime
  }

  # Maps to S_P_IDENTIFIER_VALUES  table
  define('RdSPIdentifierValue', :extends => 'RdEntity') {
      #Maps to S_P_ENTITIES table
    field 'spEntityId', :string
    field 'legalName', :string
    field 'shortName', :string
    field 'primarySIC', :string
    field 'primaryGICS', :string
    field 'primaryNAICS', :string
    field 'primaryInsuranceNAIC', :string
    field 'TSEC', :string
    field 'CUSIP', :string
    field 'CINS', :string
    field 'ticker', :string
    field 'effectiveFrom', :datetime
    field 'effectiveTo', :datetime
  }

  # Maps to S_P_RATING_VALUES  table
  define('RdSPRatingValue', :extends => 'RdEntity') {
    field 'spEntityId', :string
    # English translation corresponding to the lookup
    field 'typeId', :string
    # English translation corresponding to the lookup
    field 'groupCodeId', :string
    field 'rating', :string
    field 'ratingdate', :datetime
    field 'longtermCreditwatch', :string
    field 'longtermCreditwatchDate', :datetime
    field 'longTermOutlook', :string
    field 'longTermOutlookDate', :datetime
    field 'longTermRating', 'RdAgentRating'
    field 'longTermRatingDate', :datetime
    field 'shorttermCreditwatch', :string
    field 'shorttermCreditwatchDate', :datetime
    field 'shortTermOutlook', :string
    field 'shortTermOutlookDate', :datetime
    field 'shortTermRating', 'RdAgentRating'
    field 'shortTermRatingDate', :datetime
    field 'effectiveFrom', :datetime
    field 'effectiveTo', :datetime
  }

# Maps to S_P_ENTITIES  table
  define('RdSPEntity', :extends => 'RdEntity') {
    field 'spEntityIdentifier', :string
    field 'publishedName', :string
    field 'statusId', :string
    field 'lastUpdateDate', :datetime

    field 'spRatingValues', :list, :element_type => 'RdSPRatingValue'
    field 'spIdentifierValues', :list, :element_type => 'RdSPIdentifierValue'
    field 'spEndorsementValues', :list, :element_type => 'RdSPEndorsementValue'
    field 'spFlagValues', :list, :element_type => 'RdSPFlagValue'
    field 'spLocationValues', :list, :element_type => 'RdSPLocationValue'
    field 'spRegionValues', :list, :element_type => 'RdSPRegionValue'
    field 'spSectorValues', :list, :element_type => 'RdSPSectorValue'
  }

  # Maps to TRAFIGURA_RISK_RATING_VALUES table
  define('RdTrafiguraRiskRatingValue', :extends => 'RdEntity') {
    field 'entityId', :string
    # Maps to AGENT_RATINGS table
    field 'agentRating', 'RdAgentRating'
    field 'modifiesBy', :string
    field 'effectiveFrom', :datetime
    field 'effectiveTo', :datetime
    field 'modifiedBy', :string
  }

}