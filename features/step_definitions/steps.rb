# encoding: utf-8

Given /^xreq is in the PATH$/ do
  #system("make bin/xreq");
  if ENV['mode'] then
    libdir = ENV['mode']
  else
    libdir = 'debug'
  end
  ENV['LIBRARY_PATH']     = "#{$xreq_dir}/lib/#{libdir}:#{ENV['LIBRARY_PATH']}";
  ENV['LD_LIBRARY_PATH']  = "#{$xreq_dir}/lib/#{libdir}:#{ENV['LD_LIBRARY_PATH']}";
  ENV['PATH']             = "#{$xreq_dir}/bin:#{ENV['PATH']}";
  ENV['ADA_INCLUDE_PATH'] = "#{$xreq_dir}:#{$xreq_dir}/src/lib:#{ENV['ADA_INCLUDE_PATH']}";
  ENV['C_INCLUDE_PATH']   = "#{$xreq_dir}/src/lib:#{ENV['C_INCLUDE_PATH']}";
  ENV['GPR_PROJECT_PATH'] = "#{$xreq_dir}:#{ENV['GPR_PROJECT_PATH']}";
end

Given /^the sources of xreq are in ADA_INCLUDE_PATH$/ do
  ENV['ADA_INCLUDE_PATH'] = "#{$xreq_dir}/src:#{ENV['ADA_INCLUDE_PATH']}";
end

Given /^"([^"]*)" is empty$/ do |dir|
  Dir.foreach(dir) do |f|
    if f == '.' or f == '..' then next end
    FileUtils.rm_rf("#{dir}/#{f}")
  end
end

Given /^I am in an empty directory$/ do
  @oldcwd = FileUtils.pwd();
  dir = "#{$xreq_dir}/tmp";
  FileUtils.cd($xreq_dir);
  FileUtils.remove_dir(dir) rescue nil;
  FileUtils.mkdir_p(dir);
  FileUtils.cd(dir);
  #puts FileUtils.pwd();
end

Given /^I am in the xreq directory$/ do
  @oldcwd = FileUtils.pwd();
  FileUtils.cd($xreq_dir);
end

Given /^I am in "(.*)"$/ do |dir|
  @oldcwd = FileUtils.pwd();
  FileUtils.cd(dir);
end

Given /^a file "(.*)":$/ do |filename, filecontent|
  FileUtils::mkdir_p(File.dirname(filename));
  f = File.new(filename, "w");
  f.write(filecontent);
  f.close();
  # File.open(filename, Cucumber.file_mode('r')).read.should == filecontent;
end

When /^I run xreq (.*)$/ do |args|
  cmd = "xreq " + args + " 2>&1";
  #puts cmd;
  #puts FileUtils.pwd();
  @last_command_output = `#{cmd}`;
  @last_exit_code = $?.to_i;
  # puts @last_command_output
end

When /^I print the last command output$/ do
  puts @last_command_output;
end

When /^I print the last exit code$/ do
  puts @last_exit_code;
end

When /^I run '(.*)'(?: and save its output)?$/ do |command|
  @last_command_output = `#{command} 2>&1`;
  @last_exit_code = $?.to_i;
end

When /^I run "(.*)"$/ do |command|
  @last_command_output = `#{command} 2>&1`;
  @last_exit_code = $?.to_i;
end

When /^I run "(.*)" in (.*)$/ do |command, dir|
  olddir = FileUtils.pwd();
  #puts olddir
  FileUtils.cd(dir);
  @last_command_output = `#{command} 2>&1`;
  @last_exit_code = $?.to_i;
  FileUtils.cd(olddir);
end

When /^I run the test suite "([^"]*)" in (.*)$/ do |command, dir|
  When("I run \"#{command} --no-color\" in #{dir}")
  @last_command_output.sub!(/Finished in ([0-9]*m )?[0-9]*s\n$/, "");
end

When /^I run the test suite "([^"]*)"$/ do |command|
  When("I run \"#{command} --no-color\"")
  @last_command_output.sub!(/Finished in ([0-9]+m )?[0-9]+s\n$/, "");
end

When /^I run '(.*)' aloud$/ do |command|
  system(command);
  @last_exit_code = $?.to_i;
end

When /^I run '(.*)' silently$/ do |command|
  foo = `#{command} 2>&1`;
  @last_exit_code = $?.to_i;
end

When /^I print (.*)$/ do |str|
  puts str;
end

When /^I compile "(.*)" in (.*)$/ do |name, dir|
  if ENV['COVERAGE'] then
    n = 0
    while File.exists?("#{ENV['COVERAGE']}/#{n}.lcov.info") do
      n = n + 1
    end
    puts "Create: #{ENV['COVERAGE']}/#{n}.lcov.info"
    system("lcov -q -c -d '#{ENV['COV_OBJ_DIR']}' -t Cucumber -o '#{ENV['COVERAGE']}/#{n}.lcov.info'");
    system("lcov -q -d '#{ENV['COV_OBJ_DIR']}' --zerocounters");
  end
  FileUtils::mkdir_p(dir);
  FileUtils::rm_rf("#{dir}/obj");
  FileUtils::mkdir_p("#{dir}/obj");
  f = File.new("#{dir}/main.gpr", "w");
  f.write("with \"xreqlib.gpr\";\n");
  f.write("project Main is\n");
  f.write("   for Main        use (\"#{name}\");\n");
  f.write("   for Source_Dirs use (\".\", \"../step_definitions\");\n");
  f.write("   for Object_Dir  use \"obj\";\n");
  f.write("   for Exec_Dir    use \".\";\n");
  f.write("   package Compiler is\n");
  f.write("      for Default_Switches (\"Ada\") use (\"-gnat05\", \"-g\");\n");
  f.write("   end Compiler;\n");
  f.write("   package Binder is\n");
  f.write("      for Default_Switches (\"Ada\") use (\"-E\");\n");
  f.write("   end Binder;\n");
  f.write("end Main;\n");
  f.close();
  #command="gnatmake #{ENV['GNAT_FLAGS']} -gnat05 -g -aI../step_definitions #{name}"
  command="gnatmake #{ENV['GNAT_FLAGS']} -Pmain.gpr 2>&1"
  #puts command
  When("I run \"#{command}\" in #{dir}")
end

Then /^it should (fail|pass)$/ do |success|
  if success == 'fail'
    if @last_exit_code == 0
      raise "Unexpected Success\nOutput:\n#{@last_command_output}\n"
    end
  else
    if @last_exit_code != 0
      raise "Failed with exit status #{@last_exit_code}\nOutput:\n#{@last_command_output}\n"
    end
  end
end

Then /^it should (fail|pass) with$/ do |success, output|
  @last_command_output.should == output
  Then("it should #{success}")
end

Then /^the output should contain$/ do |text|
  @last_command_output.should include(text)
end

Then /^the output should not contain$/ do |text|
  @last_command_output.should_not include(text)
end

Then /^the output should be$/ do |text|
  @last_command_output.should == text
end

Then /^the output should match$/ do |text|
  @last_command_output.should =~ Regexp.new(text)
end

Then /^"([^\"]*)" should match "([^\"]*)"$/ do |file, text|
  File.open(file, Cucumber.file_mode('r')).read.should =~ Regexp.new(text)
end

Then /^"([^\"]*)" should contain$/ do |file, text|
  File.open(file, Cucumber.file_mode('r')).read.should include(text)
end

Then /^print output$/ do
  puts @last_command_output
end

Then /^"([^\"]*)" should exist$/ do |file|
  File.exists?(file).should be_true
end

Then /^"([^\"]*)" (?:should not|shouldn't) exist$/ do |file|
  File.exists?(file).should be_false
end
