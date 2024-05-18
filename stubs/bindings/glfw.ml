open Ctypes

module Types = struct
  (* 1365 *)
  type glproc = unit -> unit
  let glproc: glproc typ =
    typedef (Foreign.funptr (void @-> returning void))
    "GLFWglproc"

  (* 1391 *)
  type monitor
  let monitor: monitor structure typ =
    typedef (structure "GLFWmonitor") "GLFWmonitor"

  (* 1403 *)
  type window
  let window: window structure typ =
    typedef (structure "GLFWwindow") "GLFWwindow"

  (* 1590 *)
  type error_fun = int -> string -> unit
  let error_fun: error_fun typ =
    typedef
      (Foreign.funptr (int @-> string @-> returning void))
      "GLFWerrorfun"
end

module Bindings (F: Ctypes.FOREIGN) = struct
  open Types
  open F

  (* 2220 *)
  let init = foreign "glfwInit" (void @-> returning int)

  (* 2254 *)
  let terminate = foreign "glfwTerminate" (void @-> returning void)

  (* 2503 *)
  let set_error_callback = foreign "glfwSetErrorCallback" (error_fun @-> returning error_fun)

  (* 3054 *)
  let window_hint = foreign "glfwWindowHint" (int @-> int @-> returning void)

  (* 3235 *)
  let create_window = foreign "glfwCreateWindow"
    (int @-> int @-> string @-> ptr_opt monitor @-> ptr_opt window @-> returning (ptr window))

  (* 3264 *)
  let destroy_window = foreign "glfwDestroyWindow" (ptr window @-> returning void)

  (* 6109 *)
  let make_context_current = foreign "glfwMakeContextCurrent" (ptr window @-> returning void)

  (* 6290 *)
  let get_proc_address = foreign "glfwGetProcAddress" (string @-> returning glproc)
end

