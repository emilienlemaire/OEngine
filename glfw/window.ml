type t = Stubs.Glfw.Types.window Ctypes.structure Ctypes.ptr
type opengl_profile = Any | Core | Compat

let opengl_profile_to_glfw_constant = function
  | Any -> Stubs.Glfw.opengl_any_profile
  | Core -> Stubs.Glfw.opengl_core_profile
  | Compat -> Stubs.Glfw.opengl_compat_profile

type _ window_hint =
  | Context_version_major : int window_hint
  | Context_version_minor : int window_hint
  | Opengl_profile : opengl_profile window_hint

let window_hint : type hint. hint window_hint -> hint -> unit = function
  | Context_version_major ->
      fun version ->
        Stubs.Glfw.window_hint Stubs.Glfw.context_version_major version
  | Context_version_minor ->
      fun version ->
        Stubs.Glfw.window_hint Stubs.Glfw.context_version_minor version
  | Opengl_profile ->
      fun profile ->
        Stubs.Glfw.window_hint Stubs.Glfw.opengl_profile
        @@ opengl_profile_to_glfw_constant profile

let make ~width ~height ~title ?monitor ?share () =
  Stubs.Glfw.create_window width height title monitor share

let as_ptr = Fun.id
let of_ptr = Fun.id
let destroy w = Stubs.Glfw.destroy_window (as_ptr w)
