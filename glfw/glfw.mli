type t

val make : unit -> t Core.Error.t
val init : t -> t Core.Error.t
val make_current_context : Window.t -> t -> t Core.Error.t
val set_error_callback : (int -> string -> unit) -> t -> t Core.Error.t

val set_key_callback :
  (t ->
  Events.Keys.t ->
  int ->
  Events.Actions.t ->
  Events.ModifierKeys.t list ->
  unit) ->
  t ->
  t Core.Error.t

val set_framebuffer_size_callback :
  (t -> int -> int -> unit) -> t -> t Core.Error.t

val set_swap_interval : int -> t -> t Core.Error.t
val swap_buffers : t -> t Core.Error.t
val poll_events : t -> t Core.Error.t
val window_should_close : t -> bool Core.Error.t
val set_window_should_close : bool -> t -> t Core.Error.t
val is_init : t -> bool
val current_context : t -> Window.t option
val swap_interval : t -> int

val push_layer_event_cb :
  (Events.any_event Events.t Events.event ->
  Events.any_event Events.t Events.event Core.Error.t) ->
  int ->
  t ->
  t Core.Error.t

val push_overlay_event_cb :
  (Events.any_event Events.t Events.event ->
  Events.any_event Events.t Events.event Core.Error.t) ->
  int ->
  t ->
  t Core.Error.t

val remove_event_cb : int -> t -> t Core.Error.t
val finalise : t -> unit Core.Error.t

module Window = Window
module Events = Events
