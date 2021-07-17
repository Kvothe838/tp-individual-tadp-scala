require_relative '../../../src/persistible'

class Student_Refresh
  include Persistible

  has_one String, named: :full_name
  has_one Grade_Refresh, named: :grade
end
