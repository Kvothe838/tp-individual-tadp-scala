require_relative '../../../src/persistible'

class Grade_Save
  include Persistible

  has_one Numeric, named: :value
end