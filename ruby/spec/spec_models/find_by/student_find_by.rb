require_relative '../../../src/persistible'
require_relative './person_find_by'
class Student_FindBy

  include Person_FindBy
  include Persistible

  has_one Numeric, named: :grade

  def promoted
    self.grade > 8
  end



end

class StudentInferior_FindBy < Student_FindBy
  has_one Numeric, named: :age

  def es_mayor_de_edad
    self.age > 18
  end
end