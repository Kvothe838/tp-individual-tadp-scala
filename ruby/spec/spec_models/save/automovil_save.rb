require_relative '../../../src/persistible'

class Rueda_Save
  include Persistible

  has_one String, named: :marca
end

class Automovil_Save
  include Persistible

  has_one Rueda_Save, named: :rueda
end

class Auto_Save < Automovil_Save
  has_one String, named: :marca
end