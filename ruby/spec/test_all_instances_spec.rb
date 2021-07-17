require_relative 'spec_models/all_instances/point_all_instances.rb'

describe 'test_all_instances' do

  it 'crea tres puntos, guarda dos y trae dos' do
    p1 = Point_AllInstances.new
    p1.x = 2
    p1.y = 5
    p1.save!
    p2 = Point_AllInstances.new
    p2.x = 1
    p2.y = 3
    p2.save!

    # Si no salvamos p3 entonces no va a aparecer en la lista
    p3 = Point_AllInstances.new
    p3.x = 9
    p3.y = 7

    instances = Point_AllInstances.all_instances!

    expect(instances.length).to be 2
    expect(instances.any? { |instance| instance.x == 1 && instance.y == 3 }).to be true
    expect(instances.any? { |instance| instance.x == 2 && instance.y == 5 }).to be true

    p4 = instances.first
    p4.add(p2)
    p4.save!

    instances = Point_AllInstances.all_instances!

    expect(instances.length).to be 2
    expect(instances.any? { |instance| instance.x == 1 && instance.y == 3 }).to be true
    expect(instances.any? { |instance| instance.x == 3 && instance.y == 8 }).to be true

    p2.forget!

    instances = Point_AllInstances.all_instances!

    expect(instances.length).to be 1
    expect(instances.any? { |instance| instance.x == 3 && instance.y == 8 }).to be true
  end

  it 'crea un Point y un TridimentionalPoint, pide all_instances! y devuelve 2' do
    p1 = Point_AllInstances.new
    p1.x = 2
    p1.y = 5
    p1.save!
    p2 = TridimentionalPoint_AllInstances.new
    p2.x = 1
    p2.y = 3
    p2.z = 5
    p2.save!

    point_instances = Point_AllInstances.all_instances!
    tridimentionalPoint_instances = TridimentionalPoint_AllInstances.all_instances!

    expect(point_instances.length).to be 2
    expect(point_instances.any? { |instance| instance.x == 2 && instance.y == 5 }).to be true
    expect(point_instances.any? { |instance| instance.x == 1 && instance.y == 3 && instance.z = 5 }).to be true

    expect(tridimentionalPoint_instances.length).to be 1
    expect(tridimentionalPoint_instances.any? { |instance| instance.x == 1 && instance.y == 3 && instance.z = 5 }).to be true
  end
end