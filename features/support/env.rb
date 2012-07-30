# encoding: utf-8
require 'rspec/expectations'
require 'cucumber/formatter/unicode'
require 'tempfile'

$xreq_dir = File.expand_path(File.dirname(File.dirname(File.dirname(__FILE__))))

Before do
  $saved_dir = FileUtils::pwd();
  FileUtils::rm_rf("#{$xreq_dir}features/data/tests");
end

After do |scenario|
  FileUtils::cd($saved_dir);
end
