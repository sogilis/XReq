# encoding: utf-8
require 'spec/expectations'
require 'cucumber/formatter/unicode'
require 'tempfile'

Before do
  $adaspec_dir = FileUtils::pwd();
end

After do |scenario|
  FileUtils::cd($adaspec_dir);
end
