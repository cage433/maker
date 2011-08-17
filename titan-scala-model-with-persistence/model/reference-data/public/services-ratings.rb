# EDM package EDM.Enterprise.Common Domains.Reference Data.Master Data.Ratings
in_namespace('commondomains.referencedata.masterdata.ratings') {
=begin
  This service provides methods for retrieving entities defined in the
  Ratings package along with methods which are returning the
  relationship between the entities defined
=end
  service('Ratings Service') {

=begin
    Gets the complete history of a TRR for an entity based on the Id of the Rating
=end
    operation('Get Historical TRR Rating By GUID', :returns => list('TrafiguraEntityRating')) {
        # The GUID for the Rating
      parameter 'guid', :guid
      # Base method params
      parameter 'baseParams', 'BaseServiceParams'
    }

=begin
    Gets the complete history of a TRR for an entity based on the ID of the Entity it is for
=end
    operation('Get Historical TRR Rating By Entity GUID', :returns => list('TrafiguraEntityRating')) {
        # The GUID of the entity for the Rating
      parameter 'guid', :guid
      # Base method params
      parameter 'baseParams', 'BaseServiceParams'
    }

=begin
    Gets the complete history of a SP rating for an entity based on the Id of the Rating
=end
    operation('Get Historical SP Rating By GUID', :returns => 'SPRatingDetail') {
        # The GUID for the Rating
      parameter 'guid', :guid
      # Base method params
      parameter 'baseParams', 'BaseServiceParams'
    }

=begin
    Gets the complete history of a SP for an entity based on the entity identifier
=end
    operation('Get Historical SP Rating By Entity Identifier', :returns => list('SPRatingDetail')) {
      # The entity identifier
      parameter 'identifier', :string
      # Base method params
      parameter 'baseParams', 'BaseServiceParams'
    }

=begin
    Gets the complete history of a Moodys rating for an entity based on the Id of the Rating
=end
    operation('Get Historical Moodys Rating By GUID', :returns => 'MoodyRating') {
        # The GUID for the Rating
      parameter 'guid', :guid
      # Base method params
      parameter 'baseParams', 'BaseServiceParams'
    }

=begin
    Gets the complete history of a Moodys rating for an entity based on the issuer number
=end
    operation('Get Historical Moodys Rating By Issuer Number', :returns => list('MoodyRating')) {
        # The issuer number
      parameter 'issuerNumber', :string
      # Base method params
      parameter 'baseParams', 'BaseServiceParams'
    }
  }
}
