require_relative '../../../src/persistible'

class Grade_Validates
  include Persistible

  has_one Numeric, named: :value
end