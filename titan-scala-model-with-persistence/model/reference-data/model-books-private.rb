in_namespace('inner.security.books') {

  define('RdReportingGroup', :extends => 'RdLookup') {
    constant 'high value books', "HVBK"
    constant 'United States books', "USBK"
  }

  define('RdBookGroup', :extends => 'RdLookup') {
    constant 'Coal', "COAL"
    constant 'Coke', "COKE"
    constant 'Crude', "CRUDE"
    constant 'Oil', "OIL"
  }

  define('RdBookStatus', :extends => 'RdLookup') {
    constant 'Closed', "CLOSE"
    constant 'Open', "OPEN"
  }

  # Maps to BOOK_BUSINESS_OWNER
  define('RdBookBusinessOwner', :extends => 'RdBusinessOwner') {
    mixin('ChildVersionable')
    field 'bookId', :string
    field 'bookVer', :integer
  }

  # Maps to BOOKS table
  define('RdBook', :extends => 'RdEntity') {
    mixin('MainVersionable')
    field 'name', :string
    field 'description', :string
    # Maps via BOOKS.BOOK_STATUS_ID field to CODE_LOOKUPS table
    field 'statusId', :string
    field 'specBook', :boolean
    # Maps via BOOKS.ORGANISATIONAL_BOOK_GROUP_ID field to CODE_LOOKUPS table
    field 'groupId', :string
    # Maps via BUSINESS_LINE_BOOKS table
    field 'businessLinesIds', :list, :element_type => :string
    # Maps via REPORTING_BOOK_GROUP_BOOKS table to CODE_LOOKUPS table
    field 'reportingGroupIds', :list, :element_type => :string
    # Maps via BOOK_BUSINESS_OWNER table to USERS table
    field 'businessOwners', :list, :element_type => 'RdBookBusinessOwner'
  }

}