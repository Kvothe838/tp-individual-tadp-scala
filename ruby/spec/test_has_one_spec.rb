require_relative 'spec_models/has_one/person_has_one.rb'

describe 'test_has_one' do
  it 'Agrega atributos persistibles correctamente' do
    atributos = Person_HasOne.attr_persistibles.atributos
    expect(atributos.length).to be 4
    expect(atributos[0].type).to be String
    expect(atributos[0].named).to be :first_name
    expect(atributos[1].type).to be String
    expect(atributos[1].named).to be :last_name
    expect(atributos[2].type).to be Numeric
    expect(atributos[2].named).to be :age
    expect(atributos[3].type).to be Boolean
    expect(atributos[3].named).to be :admin
  end

  it 'Pisa el tipo del atributo repetido' do
    class Person_HasOne
      has_one String, named: :age
    end

    atributos = Person_HasOne.attr_persistibles.atributos

    expect(atributos[2].type).to be String
  end
end