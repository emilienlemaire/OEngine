type t

val create: unit -> t

(* TODO: Move t at the end so we can pipe it *)
val clear_color: t -> float -> float -> float -> float -> t Core.Error.t
val clear: t -> t Core.Error.t

val draw_indexed: t -> Vertex_array.t -> t Core.Error.t
