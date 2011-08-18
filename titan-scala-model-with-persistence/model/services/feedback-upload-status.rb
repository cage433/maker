in_namespace('Feedback') {

  define('Upload Status') {
    field 'host', :string
    field 'depth',       :integer
    field 'book id',     :integer
  }
  expose 'Upload Status', :id => 'host', :route_using => 'book id'
}
