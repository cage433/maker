in_namespace('mapping') {

  define('Mapping', :scala_extra_imports => ['scala.reflect.BeanProperty']) {
    field 'id',              :scala_annotations => ['@BeanProperty']
    field 'entity type',     :scala_annotations => ['@BeanProperty']
    field 'hub id',          :scala_annotations => ['@BeanProperty']
    field 'external id',     :scala_annotations => ['@BeanProperty']
    field 'external schema', :scala_annotations => ['@BeanProperty']
  }

}
