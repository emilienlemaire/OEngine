open Ctypes

module Types = struct
  type apiproc = unit -> unit
  let apiproc: apiproc typ = typedef (Foreign.funptr (void @-> returning void)) "GLADapiproc"

  type loadfunc = string -> apiproc
  let loadfunc: loadfunc typ =
    typedef (Foreign.funptr (string @-> returning apiproc)) "GLADloadfunc"
end

module Bindings (F: Ctypes.FOREIGN) = struct
  open Types
  open F

  let load_gl = foreign "gladLoadGL" (loadfunc @-> returning int)
end

