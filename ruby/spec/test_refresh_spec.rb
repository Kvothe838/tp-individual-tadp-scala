require_relative 'spec_models/refresh/person_refresh.rb'
require_relative 'spec_models/refresh/grade_refresh.rb'
require_relative 'spec_models/refresh/student_refresh.rb'
require_relative 'spec_models/refresh/automovil_refresh.rb'

describe 'test_refresh' do

  it 'resetea una persona' do
    persona = Person_Refresh.new
    persona.first_name = "Juan"
    persona.last_name = "Pérez"
    persona.age = 13
    persona.save!

    persona.first_name = "Pedro"
    persona.refresh!

    expect(persona.first_name).to eq("Juan")
  end

  it 'resetea el grade del student' do
    student = Student_Refresh.new
    student.full_name = "Juan Pérez"
    student.grade = Grade_Refresh.new
    student.grade.value = 5
    student.save!

    student.grade.value = 10
    student.refresh!

    expect(student.grade.value).to be 5
  end

  it 'crea un auto que hereda de automovil' do
    auto = Auto_Refresh.new
    auto.marca = 'VMW'
    auto.rueda = Rueda_Refresh.new
    auto.rueda.marca = 'Michelin'
    auto.save!

    auto.rueda.marca = 'Goodyear'
    auto.refresh!

    expect(auto.rueda.marca).to eq('Michelin')
  end
end