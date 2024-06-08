type t

type opengl_profile =
  | Any
  | Core
  | Compat

type _ window_hint =
  | Context_version_major : int window_hint
  | Context_version_minor : int window_hint
  | Opengl_profile : opengl_profile window_hint

val window_hint : 'a window_hint -> 'a -> unit

val make :
  width:int ->
  height:int ->
  title:string ->
  ?monitor:Stubs.Glfw.Types.monitor Ctypes.structure Ctypes.ptr ->
  ?share:t ->
  unit ->
  t

val destroy : t -> unit

val as_ptr : t -> Stubs.Glfw.Types.window Ctypes.structure Ctypes.ptr

val of_ptr : Stubs.Glfw.Types.window Ctypes.structure Ctypes.ptr -> t
