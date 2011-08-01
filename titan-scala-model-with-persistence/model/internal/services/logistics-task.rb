in_namespace('Internal.Logistics.Task') {
  service('LogisticsTaskService') {

    operation('GetAll', :returns => list('LogisticsTask')) {
    }

    operation('GetTasks', :returns => list('LogisticsTask')) {
      parameter 'guids',    list(:guid)
    }

    operation('AddTask', :returns => 'LogisticsTask') {
      parameter 'quotaId',    :string
      parameter 'reason',     :string
    }

    operation('UpdateTasks', :returns => list('LogisticsTask')) {
      parameter 'updates',    list('LogisticsTaskUpdate')
    }
  }
}
