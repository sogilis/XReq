require 'rspec/expectations'

Given /^there are ([0-9]+) cucumbers$/ do |cukes|
  @cukes = cukes.to_i
end

When /^I eat ([0-9]+) cucumbers$/ do |cukes|
  @cukes = @cukes - cukes.to_i
end

Then /^I should have ([0-9]+) cucumbers$/ do |cukes|
  cukes.to_i.should == @cukes
end
