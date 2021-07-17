require_relative '../../../src/persistible'

class Grade_Refresh
  include Persistible

  has_one Numeric, named: :value
end