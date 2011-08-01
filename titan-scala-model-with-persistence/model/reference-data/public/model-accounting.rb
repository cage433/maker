# EDM package EDM.Enterprise.Common Domains.Reference Data.Master Data.Accounting
in_namespace('commondomains.referencedata.masterdata.accounting') {
  define('AccountType') {
    field 'id', :guid
  }

=begin
  We might need to to extend this to be able to provide a specific type
  however it's unknown yet.
  TODO: have to clarify with NICK
  The shortCode field might be used as a unique identifier.
=end
#  define('AccountType', :extends => 'EnumeratedItem') {
#    # CODE_SETS.SHORT_CODE   ::  ACCO
#    # CODE_SETS.SET_DESC     ::  Account codes
#    # CODE_LOOKUPS.CL_VALUE  ::  SCRED, SDEBT
#    # CODE_LOOKUPS.CL_DESC   ::  Sun Credit Account, Sun Debtor Account
#  }

=begin
  Book type as an extension over Enumerated Item
  The shortCode field might be used as a unique identifier.
=end
  define('BookType', :extends => 'EnumeratedItem') {
      # CODE_SETS.SHORT_CODE   ::
  # CODE_SETS.SET_DESC     ::
  }

=begin
  Book entity - as an Extended enumerated type
  This entity maps to the BOOKS table
=end
  define('Book', :extends => 'Entity') {
      # NAME - The unique identifier for the record
    field 'shortCode', :string
    # DESCRIPTION - The human readable description in English
    field 'description', :string
    # The rank of the record defualts to 0
    field 'rank', :integer, :optional => true
=begin
    VER_START + VER_END - A date range when the record is active. 
    The end date can be null.
=end
    field 'effectiveDateRange', 'commontypes.dataspecifications.DateRange'
    # TODO: same fields as EnumeratedItem
    # Maps to the STATUSES table via the BOOK_STATUS_ID
    field 'status', 'Status'
    # Maps to the SPEC_BOOK field
    field 'specBook', :boolean
    # Maps to the CODE_LOOKUPS table via the BOOK_TYPE_ID
    field 'bookType', 'BookType'
=begin
    Maps to the CODE_LOOKUPS table via the BOOK_BUSINESS_OWNER table
    where BOOK_BUSINESS_OWNER.BK_ID = BOOKS.ID
      and BOOK_BUSINESS_OWNER.BK_VER = BOOKS.VER
      and CODE_LOOKUPS.ID = BOOK_BUSINESS_OWNER.OWNER_TYPE_ID
=end
    #field 'ownership', :list, :element_type => 'BusinessOwnership'
    field 'businessLines', :list, :element_type => 'BusinessLine'
  }

=begin
  Book Group as an extension over Enumerated Item
  The shortCode field might be used as a unique identifier.
=end
  define('BookGroup', :extends => 'EnumeratedItem') {
=begin
    # CODE_SETS.SHORT_CODE   ::  
    # CODE_SETS.SET_DESC     ::  
    Maps to the CODE_LOOKUPS table via the REPORTING_BOOK_GROUP_BOOKS table
    where REPORTING_BOOK_GROUP_BOOKS.BK_ID = BOOKS.ID
      and REPORTING_BOOK_GROUP_BOOKS.BK_VER = BOOKS.VER
      and CODE_LOOKUPS.ID = REPORTING_BOOK_GROUP_BOOKS.REPORTING_BOOK_GROUP_ID
=end
    field 'books', :list, :element_type => 'Book'
    field 'status', 'Status'
  }

=begin
  Desk which is a synonym for Book
=end
  define('Desk', :extends => 'Book') {}
}
