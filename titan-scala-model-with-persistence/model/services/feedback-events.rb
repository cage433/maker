in_namespace('Feedback') {

  define('Feedback Event', :abstract => true) {
    field 'operation id',   :guid       # The operation id associated with the original request
    field 'operation summary', :string  # Human-readable summary of the operation this event is involved in
    field 'initiating user',  :string   # Username of the initiating user
    field 'original id',    :string     # The application identifier for the contents of the original message
    field 'original type',  :string     # The original type of the object
    field 'caller id',      :string     # The application identifier of the original caller
    field 'handler id',     :string     # An identifier (such as a hostname) for the handling application
  }
  define('Success Feedback Event', :extends => 'Feedback Event') {
    field 'source hash',  :string     # The hash of the content that was originally provided
    field 'dest hash',    :string     # The hash of the content, as known by the destination
  }
  define('Transaction Success Feedback Event', :extends => 'Success Feedback Event') {
    field 'business date',  :datetime     # The relevant business date for the transactional entity (eg, trade date)
    field 'book',           :integer_key, :references => 'Book'   # The book that the transaction belongs to
  }
  define('Failure Feedback Event', :extends => 'Feedback Event') {
    field 'code',         :string     # A machine readable code detailing the failure
    field 'message',      :string     # A human readable message detailing the failure
  }
  
	# A list container for feedback events, so we can extend to support data binding
	define('Feedback Event List') {
		field 'feedback events',  :list,    :element_type => 'Feedback Event'
	}
	
  expose 'Feedback Event', :route_using => 'caller id'
}
