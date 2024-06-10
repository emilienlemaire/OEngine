type t

val make : (module Layer.APPLICATION_LAYER) -> t Core.Error.t

val update : t -> t Core.Error.t

val should_stop : t -> bool Core.Error.t

val finalise : t -> unit Core.Error.t

val push_layer : (module Layer.APPLICATION_LAYER) -> t -> t Core.Error.t

val push_overlay : (module Layer.APPLICATION_LAYER) -> t -> t Core.Error.t

val remove_layer : (module Layer.APPLICATION_LAYER) -> t -> t Core.Error.t

val run : t -> t Core.Error.t
