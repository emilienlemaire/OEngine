module Glfw = struct
  module Types = Bindings.Types.Glfw
  include Private.Bindings_.Glfw
  include Private.Constants_.Glfw
end

module Glew = struct
  include Private.Bindings_.Glew
  include Private.Constants_.Glew
end

module Gl = struct
  module Types = Bindings.Types.Gl
  include Private.Bindings_.Gl
  include Private.Constants_.Gl
end
