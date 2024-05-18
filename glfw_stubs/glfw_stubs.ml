module Glfw = struct
  module Types = Glfw_bindings.Types
  include Glfw_bindings.Bindings(Glfw_generated)
end
