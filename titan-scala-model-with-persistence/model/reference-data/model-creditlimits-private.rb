# this is the model for Reference Data Project
in_namespace('inner.ReferenceData.CreditLimit') {

  define('RdCreditLimitType') {
    constant 'classShortCode', 'CLT'

    field 'id',                      :string
    mixin('MainVersionable')

    field 'shortCode',               :string
    field 'description',             :string
    field 'rank',                    :integer, :optional => true

    field 'categoryId',              :string
    field 'statusId',                :string

    field 'behavioursIds',           :list, :element_type => :string
  }
}