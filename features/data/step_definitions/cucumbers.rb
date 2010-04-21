require 'test/unit/assertions'
World(Test::Unit::Assertions)

Given /^there are ([0-9]+) cucumbers$/ do |cukes|
  @cukes = cukes.to_i
end

When /^I eat ([0-9]+) cucumbers$/ do |cukes|
  @cukes = @cukes - cukes.to_i
end

Then /^I should have ([0-9]+) cucumbers$/ do |cukes|
  assert_equal (cukes.to_i, @cukes)
end
