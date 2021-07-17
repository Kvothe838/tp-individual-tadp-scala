require_relative '../../../src/persistible'

class Rueda_Refresh
  include Persistible

  has_one String, named: :marca
end

class Automovil_Refresh
  include Persistible

  has_one Rueda_Refresh, named: :rueda
end

class Auto_Refresh < Automovil_Refresh
  has_one String, named: :marca
end