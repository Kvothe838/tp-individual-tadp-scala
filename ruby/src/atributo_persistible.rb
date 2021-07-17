class AtributoPersistible
  attr_accessor :named, :type, :validations, :default

  def crear_validaciones(from, to, no_blank, validate)
    validations = [type_validation]
    validations << from_validation(from) unless from.nil?
    validations << to_validation(to) unless to.nil?
    validations << no_blank_validation if no_blank
    validations << validate_validation(validate) unless validate.nil?
    self.validations = validations
  end

  def validar!(objeto)
    var = variable_get(objeto)
    validar_elemento!(var)
  end

  def validar_elemento!(elemento)
    validations.each { |v| elemento.instance_eval(&v) }
  end

  def validate_validation(validate)
    proc do
      cumple_validacion = self.instance_eval(&validate)
      raise ValidacionIncorrectaException, 'Validación no cumplida' unless cumple_validacion
    end
  end

  def no_blank_validation
    named = self.named
    mensaje = "El atributo #{named} es vacío"
    proc do
      raise ValidacionIncorrectaException, mensaje if to_s.length.zero? || is_a?(NilClass)
    end
  end

  def from_validation(_from)
    named = self.named
    from = _from
    mensaje = "El atributo #{named} no contiene valor en los limites esperados"
    proc { raise ValidacionIncorrectaException, mensaje unless self > from || is_a?(NilClass) }
  end

  def to_validation(_to)
    named = self.named
    to = _to
    mensaje = "El atributo #{named} no contiene valor en los limites esperados"
    proc { raise ValidacionIncorrectaException, mensaje unless self < to || is_a?(NilClass) }
  end

  def type_validation_con_mensaje(mensaje)
    type = self.type
    proc do
      raise ValidacionIncorrectaException, mensaje unless is_a?(type) || is_a?(NilClass)
    end
  end

  def variable_get(objeto)
    objeto.instance_variable_get "@#{named}"
  end

  def variable_set(objeto, valor)
    objeto.instance_variable_set "@#{named}", valor
  end
end

class HasManyPersistible < AtributoPersistible
  def validar!(objeto)
    variable_get(objeto).each { |a| validar_elemento! a }
  end

  def type_validation
    named = self.named
    type = self.type
    mensaje = "El atributo #{named} no contiene todos sus elementos de clase #{type}"
    type_validation_con_mensaje mensaje
  end
end

class HasOnePersistible < AtributoPersistible
  def type_validation
    named = self.named
    type = self.type
    mensaje = "El atributo #{named} no contiene valor de clase #{type}"
    type_validation_con_mensaje mensaje
  end
end