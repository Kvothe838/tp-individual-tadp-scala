require_relative './boolean'
require_relative './atributo_persistible'

class AtributosPersistibles
  attr_accessor :atributos, :nombre

  def initialize(nombre)
    @atributos = []
    @nombre = nombre
  end

  def repite_nombre_columna(named)
    atributos.any? { |obj| obj.named == named }
  end

  def repite_columna?(columna)
    atributos.any? { |obj| obj === columna }
  end

  def agregar_columna!(columna)
    if repite_nombre_columna(columna.named)
      columna_repetida = atributos.find{ |obj| obj.named == columna.named }
      unless columna_repetida.nil?
        columna_repetida.type = columna.type
        columna_repetida.validations = columna.validations
        # columna_repetida[:relation] = columna[:relation]
        columna_repetida.default = columna.default
      end
    else
      atributos << columna
    end
  end

  def es_valor_correcto_segun_clase(valor, clase)
    (valor.nil? && !es_tipo_primitivo?(clase)) || valor.is_a?(clase)
  end

  def validar!(objeto)
    atributos.each do |atributo|
      atributo.validar!(objeto)
    end
    nil
  end

  def dame_los_many
    atributos.filter{ |a| a.is_a? HasManyPersistible }
  end

  def dame_los_one
    atributos.filter{ |a| a.is_a? HasOnePersistible }
  end

  def dame_el_hash(objeto)
    dame_los_one.inject({}) do
    |nuevo_hash,atributo|
      tipo_atributo = atributo.type
      valor = atributo.variable_get(objeto)
      if es_tipo_primitivo? tipo_atributo
        a_guardar = valor
        nuevo_hash[atributo.named] ||= a_guardar
      else
        if valor.class != NilClass
          valor.save!
          a_guardar = valor.id
          nuevo_hash[atributo.named] ||= a_guardar
        end
      end
      nuevo_hash
    end
  end

  def es_tipo_primitivo?(tipo)
    tipos_primitivos = [String, Numeric, Boolean, Integer]
    tipos_primitivos.include? tipo
  end

  def refresh!(objeto, una_fila)
    una_fila.each do |key, value|
      class_atributo = String
      if key != :id
        atributo = atributos.find { |a| a.named == key }
        class_atributo = atributo&.type
      end

      if es_tipo_primitivo? class_atributo
        objeto.instance_variable_set("@#{key}", value)
      else
        valor_aux = instanciar_por_tipo(class_atributo)
        valor_aux.id = value
        valor_aux.refresh!
        objeto.instance_variable_set("@#{key}", valor_aux)
      end
    end

    dame_los_many.each do |attribute|
      table_name = "#{objeto.class.to_s}-#{attribute.named.to_s}"
      tabla_atributo_has_many = TADB::DB.table(table_name)
      arreglo = []

      tipo = attribute.type
      if es_tipo_primitivo? tipo
        tabla_atributo_has_many.entries.each do |fila|
          arreglo.push(fila[:value])
        end
      else
        tabla_atributo_has_many.entries.each do |fila|
          # arreglo.push(fila[:value])
          valor_aux = instanciar_por_tipo(tipo)
          valor_aux.id = fila[:value]
          valor_aux.refresh!
          arreglo.push(valor_aux)
        end
      end

      objeto.instance_variable_set("@#{attribute.named}", arreglo)
    end
  end

  private

  def instanciar_por_tipo(tipo)
    Kernel.const_get(tipo.to_s.to_sym).new
  end

end