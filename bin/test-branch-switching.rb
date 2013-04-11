#!/usr/bin/ruby -w

def write_heading(cmd, msg)
  puts "\n" * 4
  puts "*" * 50 
  puts msg
  puts "Executing #{cmd}"
  puts "" 
end

def exec_command(cmd, msg="")
  write_heading(cmd, msg)
  system cmd
  result = $?.exitstatus
  puts "Result was #{result}"
  raise "Failed to execute '#{cmd}', result was #{result}" unless result == 0
end

TEMP_DIR_SUFFIX="-MAKER-TEST-BRANCH-SWITCHING"

exec_command("rm -rf /tmp/*#{TEMP_DIR_SUFFIX}", "Delete any old temp directories")

exec_command("./bin/maker.sh -b -y -d -e \"update\"", "Build clean maker") 
exec_command("./bin/maker.sh -e \"clean;test\"", "Test clean maker")

maker_dir=Dir.pwd.chomp
temp_dir=`mktemp -d --suffix=#{TEMP_DIR_SUFFIX}`.chomp
puts "Starling working directory is #{temp_dir}"
Dir.chdir(temp_dir)
exec_command("git clone -b master ssh://alex@alex-linux/~/repos/starling-snapshots", "Clone starling") 
Dir.chdir("#{temp_dir}/starling-snapshots")

branches=["stress", "tradeperf", "costsexplain", "removezeros", "nodeletedinaspect"]
branches.each do |br|
  exec_command("git checkout -b #{br} origin/#{br}")
  Dir.chdir("#{temp_dir}/starling-snapshots/starling")
  exec_command("#{maker_dir}/bin/maker.sh -d -c maker/project -p maker/Starling.scala -e \"update;test\"")
end

Dir.chdir(maker_dir)


