open Core.Syntax.Result

module type APPLICATION_LAYER = sig
  type t

  val id : int

  val attach : unit -> unit Core.Error.t

  val update : float -> unit Core.Error.t

  val detach : unit -> unit Core.Error.t

  val event :
    Events.any_event Events.t Events.event -> Events.any_event Events.t Events.event Core.Error.t
end

module type LAYER = sig
  type t

  val create : unit -> t

  (** @param state The state of the layer after creation and before attachment to the application.
      @return The state of the layer after attachment to the application. *)
  val attach : t -> t Core.Error.t

  (** @param timestep Current frame time step
      @param state Current frame layer state *)
  val update : float -> t -> t Core.Error.t

  val detach : t -> unit Core.Error.t

  val event :
    Events.any_event Events.t Events.event ->
    t ->
    (Events.any_event Events.t Events.event * t) Core.Error.t
end

