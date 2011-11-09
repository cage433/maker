in_namespace('TradeCapture.Internal.Auditing') {
  # this defines standard auditing requirements of objects 
  define('Audit') {
      field 'ver',              :integer
      field 'modifiedby',       :integer_key, :references => 'TradeCapture.Internal.PermissionService.User(oid)'
  }
}
