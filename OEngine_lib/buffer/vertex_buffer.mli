type t

val init: float list -> BufferLayout.t -> t Core.Error.t

val create: int -> BufferLayout.t -> t Core.Error.t

val bind: t -> t Core.Error.t
val unbind: t -> t Core.Error.t

val data: float list -> t -> t Core.Error.t

val delete: t -> unit Core.Error.t

val layout: t -> BufferLayout.t

