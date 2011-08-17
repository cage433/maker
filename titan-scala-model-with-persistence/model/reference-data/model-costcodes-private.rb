# this is the model for Reference Data Project
in_namespace('inner.ReferenceData.CostCodes') {
=begin

=end

 define('RdCostCodeGranularity') {
   constant 'classShortCode', 'CCGR'

   field 'id',            :string
   mixin('MainVersionable')

   field 'shortCode',     :string
   field 'description',   :string

   field 'statusId',      :string
   field 'parentId',      :string

   field 'businessLinesIds',           :list, :element_type => :string
 }

 define('RdCostCodeType') {
   constant 'classShortCode', 'CCTP'

   field 'id',            :string
   mixin('MainVersionable')

   field 'shortCode',     :string
   field 'description',   :string

   field 'statusId',      :string
   field 'parentId',      :string

   field 'businessLinesIds',           :list, :element_type => :string
 }

  define('RdCostCode') {
    constant 'classShortCode', 'COI'

    field 'id', :string
    mixin('MainVersionable')

    field 'shortCode',           :string
    field 'description',         :string


    field 'statusId',            :string
    field 'costCodeTypeId',      :string
    field 'costCodeDirectionId', :string

    field 'granularityLevelIds', :list, :element_type => :string
    field 'flagsIds',            :list, :element_type => :string
    field 'businessLinesIds',    :list, :element_type => :string
  }

}
