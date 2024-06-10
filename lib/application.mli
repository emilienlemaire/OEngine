module rec Layer : sig
  module type APPLICATION_LAYER = sig
    type t

    val id : int

    val attach : unit -> unit Core.Error.t

    val update : float -> Application.t -> unit Core.Error.t

    val detach : unit -> unit Core.Error.t

    val event :
      Events.any_event Events.t Events.event -> Events.any_event Events.t Events.event Core.Error.t
  end

  module type LAYER = sig
    type t

    val name : string

    val create : unit -> t

    (** @param state The state of the layer after creation and before attachment to the application.
        @return The state of the layer after attachment to the application. *)
    val attach : t -> t Core.Error.t

    (** @param timestep Current frame time step
        @param state Current frame layer state *)
    val update : float -> Application.t -> t -> t Core.Error.t

    val detach : t -> unit Core.Error.t

    val event :
      Events.any_event Events.t Events.event ->
      t ->
      (Events.any_event Events.t Events.event * t) Core.Error.t
  end

  module Make: LAYER -> APPLICATION_LAYER

end

and Application : sig
  type t

  val make : (module Layer.APPLICATION_LAYER) -> t Core.Error.t

  val update : t -> t Core.Error.t

  val should_stop : t -> bool Core.Error.t

  val finalise : t -> unit Core.Error.t

  val glfw : t -> Glfw.t

  val push_layer : (module Layer.APPLICATION_LAYER) -> t -> t Core.Error.t

  val push_overlay : (module Layer.APPLICATION_LAYER) -> t -> t Core.Error.t

  val remove_layer : (module Layer.APPLICATION_LAYER) -> t -> t Core.Error.t

  val run : t -> t Core.Error.t
end
