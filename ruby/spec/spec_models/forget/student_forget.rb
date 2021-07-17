require_relative '../../../src/persistible'

class Student_Forget
  include Persistible

  has_one String, named: :full_name
  has_one Grade_Forget, named: :grade
end
