in_namespace('TradeMgmt.Internal.Auditing') {
  # this defines standard auditing requirements of objects 
  define('Audit') {
      field 'ver',              :integer
      field 'modifiedby',       :integer_key, :references => 'TradeMgmt.Internal.Permission.User(oid)'
  }
}
