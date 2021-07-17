require_relative 'spec_models/find_by/student_find_by.rb'
require_relative 'spec_models/find_by/person_find_by.rb'
require_relative 'spec_models/find_by/assistant_find_by.rb'
describe 'test_find_by' do

  def comparar_students(student1, student2)
    expect(student1.id).to eq(student2.id)
    expect(student1.full_name).to eq(student2.full_name)
    expect(student1.grade).to eq(student2.grade)
    expect(student1.materias_desaprobadas).to eq(student2.materias_desaprobadas)
  end

  let(:full_name) { 'Bob Esponja' }
  let(:un_student) do
    un_student = Student_FindBy.new
    un_student.full_name = full_name
    un_student.grade = 9
    un_student.materias_desaprobadas = 21
    un_student
  end
  let(:otro_student) do
    otro_student = Student_FindBy.new
    otro_student.full_name = "Gari Esponja"
    otro_student.grade = 2
    otro_student.materias_desaprobadas = 20
    otro_student
  end
  let(:un_student_inferior) do
    un_student_inferior = StudentInferior_FindBy.new
    un_student_inferior.full_name = "Juan Esponja"
    un_student_inferior.grade = 11
    un_student_inferior.age = 19
    un_student_inferior.materias_desaprobadas = 20
    un_student_inferior
  end
  let(:otro_student_inferior) do
    otro_student_inferior = StudentInferior_FindBy.new
    otro_student_inferior.full_name = "Pedro Superman"
    otro_student_inferior.grade = 3
    otro_student_inferior.age = 17
    otro_student_inferior.materias_desaprobadas = 20
    otro_student_inferior
  end
  let(:un_asistente) do
    un_asistente = Assistant_FindBy.new
    un_asistente.full_name = "Pedro Superman"
    un_asistente.grade = 3
    un_asistente.materias_desaprobadas = 25
    un_asistente
  end

  context 'find_by_<what>' do
    before do
      un_student.save!
      otro_student.save!
    end

    it 'find_by_id entre dos elementos' do
      encontrado = Student_FindBy.find_by_id(un_student.id).first

      comparar_students(encontrado, un_student)
    end

    it 'find_by_promoted' do
      aprobado = Student_FindBy.find_by_promoted(true).first

      comparar_students(aprobado, un_student)
    end

    it 'find_by_last_name' do
      esponjas = Student_FindBy.find_by_has_last_name(true, 'Esponja')

      comparar_students(esponjas.first, un_student)
      comparar_students(esponjas.last, otro_student)
    end
  end

  context 'find_by_<what> con herencia' do
    before do
      un_student.save!
      otro_student.save!
      un_student_inferior.save!
      otro_student_inferior.save!
    end

    it 'find_by_id de un_student entre elementos de Student' do
      encontrado = Student_FindBy.find_by_id(un_student.id).first

      comparar_students(encontrado, un_student)
    end

    it 'find_by_id de un_student_inferior entre elementos de Student' do
      encontrado = Student_FindBy.find_by_id(un_student_inferior.id).first

      comparar_students(encontrado, un_student_inferior)
    end

    it 'find_by_id de un_student_inferior entre elementos de StudentInferior' do
      encontrado = StudentInferior_FindBy.find_by_id(un_student_inferior.id).first

      comparar_students(encontrado, un_student_inferior)
    end

    it 'find_by_promoted de Student' do
      aprobados = Student_FindBy.find_by_promoted(true)

      expect(aprobados.length).to be 2
    end

    it 'find_by_promoted de StudentInferior' do
      aprobados = StudentInferior_FindBy.find_by_promoted(true)

      expect(aprobados.length).to be 1
    end

    it 'find_by_last_name de Student con Esponja' do
      esponjas = Student_FindBy.find_by_has_last_name(true, 'Esponja')

      expect(esponjas.length).to be 3
      comparar_students(esponjas.first, un_student_inferior)
      comparar_students(esponjas[1], un_student)
      comparar_students(esponjas.last, otro_student)
    end

    it 'find_by_last_name de Student con Superman' do
      supermans = Student_FindBy.find_by_has_last_name(true, 'Superman')
      expect(supermans.length).to be 1

      comparar_students(supermans.first, otro_student_inferior)
    end

    it 'find_by_last_name de StudentInferior con Esponja' do
      supermans = StudentInferior_FindBy.find_by_has_last_name(true, 'Esponja')
      expect(supermans.length).to be 1

      comparar_students(supermans.first, un_student_inferior)
    end

    it 'find_by_last_name de StudentInferior con Superman' do
      supermans = StudentInferior_FindBy.find_by_has_last_name(true, 'Superman')
      expect(supermans.length).to be 1

      comparar_students(supermans.first, otro_student_inferior)
    end

    it 'find_by_es_mayor_de_edad de StudentInferior con true' do
      mayores = StudentInferior_FindBy.find_by_es_mayor_de_edad(true)
      expect(mayores.length).to be 1

      comparar_students(mayores.first, un_student_inferior)
    end

    it 'find_by_es_mayor_de_edad de StudentInferior con false' do
      menores = StudentInferior_FindBy.find_by_es_mayor_de_edad(false)
      expect(menores.length).to be 1

      comparar_students(menores.first, otro_student_inferior)
    end

    it 'find_by_es_mayor_de_edad de Student rompe' do
      expect{Student_FindBy.find_by_es_mayor_de_edad(true)}.to raise_error(NoMethodError)
    end
    it 'Herencia Superclase y Mixin' do
      un_asistente.saraza = 0
      un_asistente.save!
      personas = Person_FindBy.find_by_materias_desaprobadas_mayor_a(true, 20)

      expect(personas.size).to be 2
    end
  end
end