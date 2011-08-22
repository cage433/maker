in_namespace('inner.common') {
=begin
  Common type for all the main business entities which can be versioned.
=end
  mixin('MainVersionable') {
      # User who modified the record
    field 'modifiedBy', :string
    # The version nr
    field 'ver', :integer
    # When the record becomes effective, timestamp
    field 'verStart', :datetime
    # When the record becomes invalid, timestamp, optional
    field 'verEnd', :datetime, :optional => true
    # When the record was cancelled, timestamp, obviously optional
    field 'verCancelled', :boolean
    # When the record was created
    field 'recDate', :datetime
  }

  mixin('ChildVersionable') {
      # The version nr
    field 'ver', :integer
  }

=begin
  Main base class for entities with notes
=end
  define('RdEntity', :abstract => true) {
      # Id of the entity
    field 'id', :string
  }

}
