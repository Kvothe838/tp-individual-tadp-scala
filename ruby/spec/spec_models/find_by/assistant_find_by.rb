
require_relative '../../../src/persistible'

class Assistant_FindBy < Student_FindBy
  has_one Numeric, named: :saraza
end