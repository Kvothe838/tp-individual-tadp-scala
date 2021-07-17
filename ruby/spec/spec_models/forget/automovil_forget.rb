require_relative '../../../src/persistible'

class Rueda_Forget
  include Persistible

  has_one String, named: :marca
end

class Automovil_Forget
  include Persistible

  has_one Rueda_Forget, named: :rueda
end

class Auto_Forget < Automovil_Forget
  has_one String, named: :marca
end