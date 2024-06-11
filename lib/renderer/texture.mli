type t

val create : string -> t Core.Error.t

val bind : int -> t -> unit Core.Error.t

val finalize : t -> unit Core.Error.t
