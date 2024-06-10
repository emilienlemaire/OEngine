type kind =
  | Vertex
  | Fragment

type t

val create : unit -> t

val add_shader : kind -> string -> t -> t Core.Error.t

val create_program : t -> t Core.Error.t

val delete_shader : int -> t -> t Core.Error.t

val bind : t -> unit Core.Error.t

val unbind : t -> unit Core.Error.t

val upload_uniform_mat4 : string -> (float, 'a) Math.Mat4x4.t -> t -> unit Core.Error.t

val finalize : t -> unit Core.Error.t

