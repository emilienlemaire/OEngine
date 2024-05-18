module Glad = struct
  module Types = Glad_bindings.Types
  include Glad_bindings.Bindings(Glad_generated)
end
