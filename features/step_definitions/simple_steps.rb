# encoding: utf-8

Given /^adaspec is in the PATH$/ do
  #system("make bin/adaspec");
  ENV['PATH'] = FileUtils::pwd() + "/bin" + ":" + ENV['PATH'];
  if ENV['ADA_INCLUDE_PATH'] then
    ENV['ADA_INCLUDE_PATH'] = FileUtils::pwd() + "/lib" + ":" + ENV['ADA_INCLUDE_PATH'];
  else
    ENV['ADA_INCLUDE_PATH'] = FileUtils::pwd() + "/lib";
  end
end

Given /^the sources of adaspec are in ADA_INCLUDE_PATH$/ do
  if ENV['ADA_INCLUDE_PATH'] then
    ENV['ADA_INCLUDE_PATH'] = FileUtils::pwd() + "/src" + ":" + ENV['ADA_INCLUDE_PATH'];
  else
    ENV['ADA_INCLUDE_PATH'] = FileUtils::pwd() + "/src";
  end
end

Given /^I am in an empty directory$/ do
  @oldcwd = FileUtils.pwd();
  dir = "#{$adaspec_dir}/tmp";
  FileUtils.cd($adaspec_dir);
  FileUtils.remove_dir(dir) rescue nil;
  FileUtils.mkdir_p(dir);
  FileUtils.cd(dir);
  #puts FileUtils.pwd();
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
end

When /^I run adaspec (.*)$/ do |args|
  cmd = "adaspec " + args + " 2>&1";
  #puts cmd;
  @last_command_output = `#{cmd}`;
  @last_exit_code = $?.to_i;
end

When /^I print the last command output$/ do
  puts @last_command_output;
end

When /^I print the last exit code$/ do
  puts @last_exit_code;
end

When /^I run '(.*)'(?: and save its output)?$/ do |command|
  @last_command_output = `#{command}`;
  @last_exit_code = $?.to_i;
end

When /^I run "(.*)"$/ do |command|
  @last_command_output = `#{command}`;
  @last_exit_code = $?.to_i;
end

When /^I run "(.*)" in (.*)$/ do |command, dir|
  olddir = FileUtils.pwd();
  FileUtils.cd(dir);
  @last_command_output = `#{command}`;
  @last_exit_code = $?.to_i;
  FileUtils.cd(olddir);
end

When /^I run '(.*)' aloud$/ do |command|
  system(command);
  @last_exit_code = $?.to_i;
end

When /^I run '(.*)' silently$/ do |command|
  foo = `#{command}`;
  @last_exit_code = $?.to_i;
end

When /^I print (.*)$/ do |str|
  puts str;
end

When /^I compile "(.*)" in (.*)$/ do |name, dir|
  When("I run \"gnatmake -gnat05 -aI../step_definitions #{name}\" in #{dir}")
end

Then /^it should (fail|pass)$/ do |success|
  if success == 'fail'
    @last_exit_code.should_not == 0
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

Then /^"([^\"]*)" should match "([^\"]*)"$/ do |file, text|
  File.open(file, Cucumber.file_mode('r')).read.should =~ Regexp.new(text)
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
