server 'production@ttraflocorh182', :app, :primary => true

namespace :sproxy do
  [:start, :stop].each do |action|
    desc "#{action} sproxy"
    task action, :roles => :app do
      run "cd ~/oil/sproxy/; ./bin/#{action}.sh"
    end
  end
end
