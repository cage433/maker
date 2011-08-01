# EDM pakage EDM.Enterprise.Common Types.Data Specifications
in_namespace('commontypes.dataspecifications') {
  define('DateRange') {
    field 'start', :datetime
    field 'end', :datetime, :optional => true
  }
}

in_namespace('common') {
=begin
  Main base class for entities defined in any SRD related files
=end
  define('Entity', :abstract => true) {
      # Id of the entity
    field 'id', :guid
  }

=begin
  This object defines the user and business context for the data returned on the Public API.
  For further details please see the latest version of
  'Strategic Reference Data Subscription form' document.
=end
  mixin('BusinessContext') {
=begin
    This field returns the optional Business Lines. At this point in time the public entity
    is not defined yet, therefore it returns the private Business Line entity.
=end
  # Commented out because of limitations in the toolin
  # field 'businessLines',            :list, :element_type => 'BusinessLine', :optional => true
  }

=begin
  This entity represents the model relationships. Please keep in mind that this entity
  is just a technical helper object therefore it never contains type information
  about the entities where the returned GUIDs are belonging. The type information
  is recorded in the documentation of the given service method.
=end
  define('EntityRelationship') {
      # The parent object where the children are belonging
    field 'parent', :guid
    # The children of the parent object
    field 'children', :list, :element_type => :guid
  }

=begin
  This class represents the base parameters which are optional for any
  service method calls in the EDM. Any other parameters which are mandatory
  are specified directly on the service call.
=end
  define('BaseServiceParams') {
    field 'hierarchyStopLimit', :integer, :optional => true
    field 'asAt', :datetime, :optional => true
    field 'langShortCode', :string, :optional => true
  }
}
