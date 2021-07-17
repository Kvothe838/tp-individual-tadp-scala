require_relative '../../../src/persistible'
module Person_FindBy
  include Persistible
  has_one String, named: :full_name
  has_one Numeric, named: :materias_desaprobadas
  def has_last_name(last_name)
    self.full_name.split(' ')[1] === last_name
  end

  def materias_desaprobadas_mayor_a(_materias_desaprobadas)
    self.materias_desaprobadas > _materias_desaprobadas
  end
end