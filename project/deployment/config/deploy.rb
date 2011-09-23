# Remove all pre-defined tasks
namespaces.clear
require 'capistrano/ext/multistage'

set :stages, %w(qa prod_and_dr prod dr)
set :default_stage, "qa"


[:oil, :metals].each do |ns|
  namespace ns do
    [:stop, :start, :restart, :redeploy, :launcher, :listusers, :plistusers, :refresh_database, :run_overnight, :threaddump].each do |action|
      desc "#{action} #{ns}"
      task action, :roles => :app do
        run "cd #{ns}; ./bin/#{action}.sh"
      end
    end
  end
end

namespace :sproxy do
  [:start, :stop].each do |action|
    desc "#{action} sproxy"
    task action, :roles => :app do
      run "cd /opt/sproxy/starling/sproxy; ./bin/#{action}.sh"
    end
  end
end
