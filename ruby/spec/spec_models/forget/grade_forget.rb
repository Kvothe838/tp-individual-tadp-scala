require_relative '../../../src/persistible'

class Grade_Forget
  include Persistible

  has_one Numeric, named: :value
end