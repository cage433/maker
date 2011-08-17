in_namespace('inner.security.ratings') {

  service('Ratings Service') {

    operation('Get Trafigura Risk Ratings', :returns => list('RdTrafiguraRiskRatingValue')) {
      parameter 'entityId', :string
    }

    operation('Get Moodys Issuers', :returns => list('RdMoodysIssuer')) {
      parameter 'issuerNumber', :string
      parameter 'atDate', :datetime
    }

    operation('Get Moodys Issuer With History', :returns => 'RdMoodysIssuer') {
      parameter 'keyId', :string
    }

    operation('Get SP Entities', :returns => list('RdSPEntity')) {
      parameter 'entityIdentifier', :string
      parameter 'atDate', :datetime
    }

    operation('Get SP Entity With History', :returns => 'RdSPEntity') {
      parameter 'keyId', :string
    }

    operation('Update Trafigura Risk Rating', :returns => 'RdTrafiguraRiskRatingValue') {
      parameter 'trr', :RdTrafiguraRiskRatingValue
    }

    operation('Get Agent Ratings By Type', :returns => list('RdAgentRating')) {
      parameter 'agType', :'RdRatingType'
    }

  }

  service('Ratings Lookup Service') {
    operation('Get Moodys Outlooks', :returns => list('RdMoodysOutlook')) {}
    operation('Get Moodys Outlook By Short Code', :returns => 'RdMoodysOutlook') {
        parameter 'shortCode', :string
    }
    operation('Get Rating Types', :returns => list('RdRatingType')) {}
    operation('Get Rating Type By Short Code', :returns => 'RdRatingType') {
        parameter 'shortCode', :string
    }
    operation('Get Moodys Rating Types', :returns => list('RdMoodysRatingType')) {}
    operation('Get Moodys Rating Type By Short Code', :returns => 'RdMoodysRatingType') {
        parameter 'shortCode', :string
    }
    operation('Get SP Rating Types', :returns => list('RdSPRatingType')) {}
    operation('Get SP Rating Type By Short Code', :returns => 'RdSPRatingType'){
        parameter 'shortCode', :string
    }
    operation('Get SP Rating Group Codes', :returns => list('RdSPRatingGroupCode')) {}
    operation('Get SP Rating Group Code By Short Code', :returns => 'RdSPRatingGroupCode') {
        parameter 'shortCode', :string
    }
    operation('Get Concentration Level Types', :returns => list('inner.referencedata.concentrations.RdConcentrationLevelType')) {}
    operation('Get Concentration Level Type By Short Code', :returns => 'inner.referencedata.concentrations.RdConcentrationLevelType') {
        parameter 'shortCode', :string
    }
  }

}
