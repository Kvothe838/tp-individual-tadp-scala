require_relative '../../../src/persistible'

class Student_Save
  include Persistible

  has_one String, named: :full_name
  has_one Numeric, named: :grade
  has_one Grade_Save, named: :grade
end
