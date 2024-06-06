type t

val create : unit -> t
val set_clear_color : float -> float -> float -> float -> t -> t Core.Error.t
val clear : t -> t Core.Error.t
val submit : t -> Vertex_array.t -> t Core.Error.t
