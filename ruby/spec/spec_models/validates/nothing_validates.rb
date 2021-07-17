require_relative '../../../src/persistible'

class Nothing_Validates
  include Persistible

  has_one String, named: :sarasa
end