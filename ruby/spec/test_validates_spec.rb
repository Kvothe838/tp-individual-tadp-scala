require_relative 'spec_models/validates/grade_validates.rb'
require_relative 'spec_models/validates/student_validates.rb'
require_relative 'spec_models/validates/nothing_validates.rb'

describe 'test_validates' do
  let(:student) do
    student = Student_Validates.new
    student.full_name = 'Juan'
    student.age = 19
    grade = Grade_Validates.new
    grade.value = 5
    student.grades = [grade]
    student
  end

  it 'crea un student sin grade' do
    student.save!

    expect(student.id).not_to be_nil
  end

  it 'crea un student con grade con formato incorrecto' do
    grade = Grade_Validates.new
    grade.value = 'Test'
    student.grade = grade
    mensaje_esperado = 'El atributo value no contiene valor de clase Numeric'

    expect { student.save! }.to raise_error(ValidacionIncorrectaException, mensaje_esperado)
  end

  it 'fallo en full_name' do
    student2 = Student_Validates.new
    student2.full_name = 1
    student2.age = 19
    grade = Grade_Validates.new
    grade.value = 5
    student2.grades = [grade]
    mensaje_esperado = 'El atributo full_name no contiene valor de clase String'

    expect { student2.save! }.to raise_error(ValidacionIncorrectaException, mensaje_esperado)
  end

  it 'fallo con full_name en blanco' do
    student2 = Student_Validates.new
    student2.full_name = ''
    student2.age = 19
    mensaje_esperado = 'El atributo full_name es vacío'

    expect { student2.save! }.to raise_error(ValidacionIncorrectaException, mensaje_esperado)
  end

  it 'fallo con límites de age' do
    student2 = Student_Validates.new
    student2.full_name = 'Pepe'
    student2.age = 1
    mensaje_esperado = 'El atributo age no contiene valor en los limites esperados'

    expect { student2.save! }.to raise_error(ValidacionIncorrectaException, mensaje_esperado)
  end

  it 'fallo con el proc validate' do
    student2 = Student_Validates.new
    student2.full_name = 'Pepe'
    student2.age = 19
    grade = Grade_Validates.new
    grade.value = 5
    grade2 = Grade_Validates.new
    grade2.value = 1
    student2.grades = [grade, grade2]
    mensaje_esperado = 'Validación no cumplida'

    expect { student2.save! }.to raise_error(ValidacionIncorrectaException, mensaje_esperado)
  end

  it 'valores por defecto' do
    student2 = Student_Validates.new
    student2.full_name = 'Pepe'
    student2.age = 19
    student2.save!

    expect(student2.dni).to eq(11111)
  end

  it 'falla por tener un array de dos tipos distintos en grades' do
    student2 = Student_Validates.new
    student2.full_name = 'Pepe'
    student2.age = 19
    grade = Grade_Validates.new
    grade.value = 5
    grade2 = Nothing_Validates.new
    student2.grades = [grade, grade2]
    mensaje_esperado = 'El atributo grades no contiene todos sus elementos de clase Grade_Validates'

    expect{student2.save!}.to raise_error(ValidacionIncorrectaException, mensaje_esperado)
  end
end