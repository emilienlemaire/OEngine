open Math
open Bigarray

type t

val create : unit -> t

val set_clear_color : float -> float -> float -> float -> t -> t Core.Error.t

val clear : t -> t Core.Error.t

val begin_scene : Orthographic_camera.t -> t -> t Core.Error.t

val submit : Vertex_array.t -> Shader.t -> ?transform:(float, float32_elt) Mat4x4.t -> t -> t Core.Error.t

module Orthographic_camera = Orthographic_camera
