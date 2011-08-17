# EDM package EDM.Enterprise.Common Types.Miscellaneous
in_namespace('commontypes.miscellaneous') {
    # This entity maps to the STATUSES table
  define('Status', :extends => 'Entity') {
=begin
    This field might be used as unique identifier.
    This field maps to the ST_SHORT_CODE column
=end
    field 'shortCode', :string
    # This field maps to the ST_DESC column
    field 'description', :string
  }

=begin
  This entity maps to the COMMENTS table however keep in mind that
  the fields other than note are not present in the table but in
  the LEGA_ENTITIES table
=end
  define('Note') {
      # This should appear in the EDM
    field 'text', :string
    field 'timestamp', :datetime
    field 'creator', :string
  }

=begin
  This entity maps to the ADDRESSES table
=end
  define('PostalAddress', :extends => 'Entity') {
      # This field maps to the ADD_1 column
    field 'line', :string
  }
}
