#!/usr/bin/ruby -w

require 'fileutils'

def update_project(dir)
  puts "Doing #{dir}"
  puts "java  -Dhttp.proxyHost=localhost -Dhttp.proxyPort=3128 -Dhttp.nonProxyHosts=nexus.global.trafigura.com -jar lib/ivy/ivy.jar -settings ivysettings.xml -retrieve \"#{dir}/lib_managed/[artifact]-[type]-[revision].[ext]\" -ivy #{dir}/ivy.xml -sync"
  puts `java  -Dhttp.proxyHost=localhost -Dhttp.proxyPort=3128 -Dhttp.nonProxyHosts=nexus.global.trafigura.com -jar lib/ivy/ivy.jar -settings ivysettings.xml -retrieve \"#{dir}/lib_managed/[artifact]-[type]-[revision].[ext]\" -ivy #{dir}/ivy.xml -sync`
end

dirs = if ARGV.size > 0 then
  ARGV
else
  dirs = `find . -name ivy.xml`.collect{ |d| File.dirname(d.chomp)}
end

dirs.each do |dir|
  update_project(dir)
end
