open Core.Syntax.Result

module type APPLICATION_LAYER = sig
  type t

  val id : int

  val attach : unit -> unit Core.Error.t

  val update : unit -> unit Core.Error.t

  val detach : unit -> unit Core.Error.t

  val event :
    Events.any_event Events.t Events.event -> Events.any_event Events.t Events.event Core.Error.t
end

module type LAYER = sig
  type t

  val create : unit -> t

  val attach : t -> t Core.Error.t

  val update : t -> t Core.Error.t

  val detach : t -> unit Core.Error.t

  val event :
    Events.any_event Events.t Events.event ->
    t ->
    (Events.any_event Events.t Events.event * t) Core.Error.t
end

let id = ref 0

let get_new_id () =
  let new_id = !id in
  incr id;
  new_id

module Make (Layer : LAYER) : APPLICATION_LAYER = struct
  type t = Layer.t

  let state : t ref = ref (Layer.create ())

  let id = get_new_id ()

  let attach () =
    let+ s = Layer.attach !state in
    state := s

  let update () =
    let+ s = Layer.update !state in
    state := s

  let detach () = Layer.detach !state

  let event e =
    let+ e, s = Layer.event e !state in
    state := s;
    e
end
