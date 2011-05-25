generate_language 'scala' do
  with_binding 'amqp', :using => 'json'
end
