module Boolean
  def self.is_a?(una_clase)
    Boolean.to_s == una_clase.to_s
  end
end

class FalseClass
  include Boolean
end

class TrueClass
  include Boolean
end