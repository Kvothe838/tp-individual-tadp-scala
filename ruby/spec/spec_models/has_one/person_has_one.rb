require_relative '../../../src/boolean'
require_relative '../../../src/persistible'

class Person_HasOne
  include Persistible

  has_one String, named: :first_name
  has_one String, named: :last_name
  has_one Numeric, named: :age
  has_one Boolean, named: :admin

  def initialize
    super
    self.admin = false
  end
end