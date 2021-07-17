require 'rspec'
require 'tadb'
require 'pry'
require_relative '../src/persistible'

RSpec.configure do |config|
  config.before(:each) do
    TADB::DB.clear_all
  end
  config.after(:all) do
    TADB::DB.clear_all
  end
end
