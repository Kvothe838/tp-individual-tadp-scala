require_relative 'spec_models/forget/person_forget.rb'
require_relative 'spec_models/forget/grade_forget.rb'
require_relative 'spec_models/forget/student_forget.rb'
require_relative 'spec_models/forget/automovil_forget.rb'

describe 'test_forget' do
  it 'crea una persona' do
    persona = Person_Forget.new
    persona.first_name = "Juan"
    persona.last_name = "Pérez"
    persona.age = 13
    persona.save!
    persona.forget!

    expect(persona.id).to be_nil
  end
end