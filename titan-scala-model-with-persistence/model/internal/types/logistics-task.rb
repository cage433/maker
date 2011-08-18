in_namespace('Internal.Logistics.Task') {
  define('LogisticsTask') {
    field 'guid',                   :guid_id, :identifier => true
    field 'neptuneQuotaId',         :string
    field 'reason',                 :string
    field 'createDate',             :datetime
    field 'version',                :integer
    field 'status',                 :enum, :enumerated => ['ACTIVE', 'REVIEWED']
    field 'reviewer',               :integer, :optional => true        # :references => 'User(oid)'
    field 'reviewDate',             :datetime, :optional => true     
  }

  define('LogisticsTaskUpdate') {
    field 'taskGuid',               :guid
    field 'reviewed',               :boolean
  }
}
