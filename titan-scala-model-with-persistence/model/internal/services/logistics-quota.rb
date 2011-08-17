in_namespace('Internal.Logistics.Quota') {
  service('LogisticsQuotaService') {
    operation('GetQuotas', :returns => list('LogisticsQuota')) {
    }

    operation('GetQuota', :returns => :LogisticsQuota) {
      parameter 'quotaId',    :integer
    }

		operation('SetComments', :returns => :LogisticsResponse) {
      parameter 'quotaId',            :integer
      parameter 'comments',           :string
      parameter 'auditVer',						:integer
    }
  }
}

