generate_language 'scala' do
  with_overlay 'defaults.rb'
  with_overlay 'refined-metal-trade-overlay.rb'
  with_overlay 'test-overlay.rb'

  with_binding 'amqp', :using => 'json'
  with_binding 'http'
  with_option 'hibernate-annotations'
  with_base_namespace 'com.trafigura'
end

generate_language 'service_metadata' do
   # requires emput loop otherwise permissions are not genterated
end
