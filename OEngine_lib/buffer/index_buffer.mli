type t

val create : int list -> t Core.Error.t
val count : t -> int
val bind : t -> t Core.Error.t
val unbind : t -> t Core.Error.t
val data : int list -> t -> t Core.Error.t
val delete : t -> unit Core.Error.t
