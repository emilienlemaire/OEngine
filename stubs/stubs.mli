module Glfw : sig
  module Types = Bindings.Types.Glfw

  include module type of Private.Bindings_.Glfw

  include module type of Private.Constants_.Glfw
end

module Glew : sig
  include module type of Private.Bindings_.Glew

  include module type of Private.Constants_.Glew
end

module Gl : sig
  module Types = Bindings.Types.Gl

  include module type of Private.Bindings_.Gl

  include module type of Private.Constants_.Gl
end
