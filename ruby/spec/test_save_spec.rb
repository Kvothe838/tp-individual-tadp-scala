require_relative 'spec_models/save/person_save.rb'
require_relative 'spec_models/save/grade_save.rb'
require_relative 'spec_models/save/student_save.rb'
require_relative 'spec_models/save/automovil_save.rb'

describe 'test_save' do
  let(:persona) do
    persona = Person_Save.new
    persona.first_name = "Juan"
    persona.last_name = "Pérez"
    persona.age = 13
    persona
  end

  let(:student) do
    student = Student_Save.new
    student.full_name = "Juan Pérez"
    student.grade = Grade_Save.new
    student.grade.value = 5
    student
  end

  it 'crea una persona' do
    persona.save!

    expect(persona.id).not_to be_nil
  end

  it 'crea un student con grade' do
    student.save!

    expect(student.id).not_to be_nil
    expect(student.grade.id).not_to be_nil
  end

  it 'crea un auto que hereda de automovil' do
    auto = Auto_Save.new
    auto.marca = 'VMW'
    auto.rueda = Rueda_Save.new
    auto.rueda.marca = 'Michelin'
    auto.save!

    expect(auto.id).not_to be_nil
    expect(auto.rueda.id).not_to be_nil
  end
end