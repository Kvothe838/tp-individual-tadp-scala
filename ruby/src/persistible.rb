# frozen_string_literal: true
require_relative 'atributos_persistibles'
require_relative './persistible_class_methods'
require_relative './validacion_incorrecta_exception'
require 'pry'

# Mixin necesario para la clase que desee implementar un ORM.
module Persistible

  attr_accessor :id

  def self.included(klass)
    klass.extend(ClassMethods)
  end

  def initialize
    attr_persistibles.dame_los_many.each do |attribute|
      self.instance_variable_set("@#{attribute.named}", [])
    end

    attr_persistibles.atributos.each do |attribute|
      valor_atributo = instance_variable_get("@#{attribute.named}")
      if !attribute.default.nil? && valor_atributo.nil?
        self.instance_variable_set("@#{attribute.named}", attribute.default)
      end
    end
  end

  def save!
    return if attr_persistibles.nil?

    validar!

    table.delete(@id) unless @id.nil?

    @id = table.insert(attr_persistibles.dame_el_hash(self))

    attr_persistibles_has_many = attr_persistibles.dame_los_many
    attr_persistibles_has_many.each do |attribute|
      table_name = "#{self.class.to_s}-#{attribute.named}"
      values = instance_variable_get("@#{attribute.named}")

      next if values.nil?

      hash_many = Hash[:id, @id]
      una_tabla = TADB::DB.table(table_name)

      if attr_persistibles.es_tipo_primitivo? attribute.type #Es primitivo
        values.each do |value|
          otro_hash = hash_many.merge(Hash[:value, value])
          una_tabla.insert(otro_hash)
        end
      else
        values.each do |value|
          value.save!
          otro_hash = hash_many.merge(Hash[:value, value.id])
          una_tabla.insert(otro_hash)
        end
      end
    end
  end

  def refresh!
    if @id.nil?
      puts 'Este objeto no tiene id'
      return
    end
    una_fila = self.class.find_by_id_from_table(@id)
    attr_persistibles.refresh!(self, una_fila)
  end

  def forget!
    table.delete(@id)
    @id = nil
  end

  def ejecutar_proc(proc)
    self.instance_eval(&proc)
  end

  private

  def validar!
    attr_persistibles.validar!(self)
  end

  def attr_persistibles
    self.class.attr_persistibles
  end

  def table
    self.class.table
  end
end