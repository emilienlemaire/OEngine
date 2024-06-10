open Stubs
open Result
open Core.Error
open Core.Syntax.Result
open Events
module Window = Window

type t =
  { is_init : bool;
    current_context : Window.t option;
    swap_interval : int;
    error_cb : (int -> string -> unit) option;
    framebuffer_size_cb : (t -> int -> int -> unit) option;
    event_callbacks :
      (int * (Events.any_event Events.t event -> Events.any_event Events.t event Core.Error.t)) list;
    key_cb : (t -> Keys.t -> int -> Actions.t -> ModifierKeys.t list -> unit) option
  }
[@@warning "-69"]
(* Unused field: they are used to avoid the GC to delete them *)

let error e = error (Glfw e)

let no_context func =
  let str = Printexc.raw_backtrace_to_string (Printexc.get_callstack 10) in
  error (No_context (func ^ "\n" ^ str))

let make () =
  ok
    { is_init = false;
      current_context = None;
      swap_interval = 0;
      error_cb = None;
      framebuffer_size_cb = None;
      event_callbacks = [];
      key_cb = None
    }

let init state =
  if Glfw.init () <> 0 then
    ok { state with is_init = true }
  else
    error Initialization_failed

let iter_event s e =
  ignore
  @@ List.fold_left
       (fun e (_, cb) ->
         let* e in
         cb e )
       (ok e) s.event_callbacks

(* to disable the GC on this functions *)
let window_size_cb = ref None

let window_close_cb = ref None

let key_cb = ref None

let mouse_button_cb = ref None

let scroll_cb = ref None

let cursor_pos_cb = ref None

let make_current_context window state =
  Glfw.make_context_current (Window.as_ptr window);
  let state = { state with current_context = Some window } in
  let _f =
    window_size_cb :=
      Some
        (fun ptr width height ->
          let usr_ptr : t = Ctypes.Root.get @@ Glfw.get_window_user_pointer ptr in
          let event = Events.create @@ WindowResize (WindowResize.create ~width ~height) in
          iter_event usr_ptr event );
    Glfw.set_window_size_callback (Window.as_ptr window) @@ Option.get !window_size_cb
  in
  let _f =
    window_close_cb :=
      Some
        (fun ptr ->
          let usr_ptr : t = Ctypes.Root.get @@ Glfw.get_window_user_pointer ptr in
          let event = Events.create @@ WindowClose (WindowClose.create ()) in
          iter_event usr_ptr event );
    Glfw.set_window_close_callback (Window.as_ptr window) (Option.get !window_close_cb)
  in
  let _f =
    key_cb :=
      Some
        (fun ptr key scancode action mods ->
          let usr_ptr : t = Ctypes.Root.get @@ Stubs.Glfw.get_window_user_pointer ptr in
          let event =
            match Actions.of_glfw action with
            | Actions.PRESS ->
              KeyPressed
                (KeyPressed.create ~key:(Keys.of_glfw key) ~modifiers:(ModifierKeys.of_many mods))
            | RELEASE ->
              KeyReleased
                (KeyReleased.create ~key:(Keys.of_glfw key) ~modifiers:(ModifierKeys.of_many mods))
            | REPEAT ->
              KeyRepeated
                (KeyRepeated.create ~key:(Keys.of_glfw key) ~modifiers:(ModifierKeys.of_many mods))
          in
          iter_event usr_ptr (Events.create event);
          match usr_ptr.key_cb with
          | Some cb ->
            cb usr_ptr (Keys.of_glfw key) scancode (Actions.of_glfw action)
              (ModifierKeys.of_many mods)
          | None -> () );
    Glfw.set_key_callback (Window.as_ptr window) (Option.get !key_cb)
  in
  let _f =
    mouse_button_cb :=
      Some
        (fun ptr button action mods ->
          let usr_ptr : t = Ctypes.Root.get @@ Glfw.get_window_user_pointer ptr in
          let event =
            match Actions.of_glfw action with
            | Actions.PRESS | REPEAT ->
              MouseButtonPressed
                (MouseButtonPressed.create ~button:(MouseButtons.of_glfw button)
                   ~modifiers:(ModifierKeys.of_many mods) )
            | RELEASE ->
              MouseButtonReleased
                (MouseButtonReleased.create ~button:(MouseButtons.of_glfw button)
                   ~modifiers:(ModifierKeys.of_many mods) )
          in
          iter_event usr_ptr (Events.create event) );
    Glfw.set_mouse_button_callback (Window.as_ptr window) (Option.get !mouse_button_cb)
  in
  let _f =
    scroll_cb :=
      Some
        (fun ptr x_offset y_offset ->
          let usr_ptr : t = Ctypes.Root.get @@ Glfw.get_window_user_pointer ptr in
          let event = Events.create @@ MouseScrolled (MouseScrolled.create ~x_offset ~y_offset) in
          iter_event usr_ptr event );
    Glfw.set_scroll_callback (Window.as_ptr window) (Option.get !scroll_cb)
  in
  let _f =
    cursor_pos_cb :=
      Some
        (fun ptr x y ->
          let usr_ptr : t = Ctypes.Root.get @@ Glfw.get_window_user_pointer ptr in
          let event = Events.create @@ MouseMoved (MouseMoved.create ~x ~y) in
          iter_event usr_ptr event );
    Glfw.set_cursor_pos_callback (Window.as_ptr window) (Option.get !cursor_pos_cb)
  in
  Glfw.set_window_user_pointer (Window.as_ptr window) @@ Ctypes.Root.create state;
  ok state

let update_usr_ptr s =
  match s.current_context with
  | None -> no_context __FUNCTION__
  | Some w ->
    Glfw.set_window_user_pointer (Window.as_ptr w) @@ Ctypes.Root.create s;
    ok s

let update_usr_ptr_opt s =
  match s.current_context with
  | None -> ok s
  | Some w ->
    Glfw.set_window_user_pointer (Window.as_ptr w) @@ Ctypes.Root.create s;
    ok s

let set_error_callback cb s =
  let _f = Glfw.set_error_callback cb in
  update_usr_ptr_opt { s with error_cb = Some cb }

let set_key_callback cb s =
  match s.current_context with
  | Some _ -> update_usr_ptr { s with key_cb = Some cb }
  | None -> no_context __FUNCTION__

let set_framebuffer_size_callback cb s =
  match s.current_context with
  | Some c ->
    let _f =
      Glfw.set_framebuffer_size_callback (Window.as_ptr c) (fun ptr width height ->
          let usr_ptr : t = Ctypes.Root.get @@ Stubs.Glfw.get_window_user_pointer ptr in
          cb usr_ptr width height )
    in
    update_usr_ptr { s with framebuffer_size_cb = Some cb }
  | None -> no_context __FUNCTION__

let set_swap_interval interval state =
  Stubs.Glfw.swap_interval 1;
  let state = { state with swap_interval = interval } in
  match state.current_context with
  | Some c ->
    Glfw.set_window_user_pointer (Window.as_ptr c) @@ Ctypes.Root.create state;
    ok state
  | None -> ok state

let get_current_context_size s =
  match s.current_context with
  | Some c ->
    let open Ctypes in
    let width = allocate int 0 in
    let heigth = allocate int 0 in
    Stubs.Glfw.get_window_size (Window.as_ptr c) width heigth;
    ok (!@width, !@heigth)
  | None -> no_context __FUNCTION__

let window_should_close s =
  match s.current_context with
  | Some c -> ok (Stubs.Glfw.window_should_close (Window.as_ptr c) <> 0)
  | None -> no_context __FUNCTION__

let set_window_should_close b s =
  match s.current_context with
  | Some c ->
    Stubs.Glfw.set_window_should_close (Window.as_ptr c) (Bool.to_int b);
    ok s
  | None -> no_context __FUNCTION__

let swap_buffers s =
  match s.current_context with
  | Some c ->
    Stubs.Glfw.swap_buffers (Window.as_ptr c);
    ok s
  | None -> no_context __FUNCTION__

let get_key key s =
  match s.current_context with
  | Some c ->
      let key_num = Events.Keys.to_glfw key in
      let ptr = Window.as_ptr c in
      ok @@ Events.Actions.of_glfw (Stubs.Glfw.get_key ptr key_num)
  | None -> error (No_context __FUNCTION__)

let poll_events s =
  Glfw.poll_events ();
  ok s

let push_layer_event_cb cb id s =
  update_usr_ptr { s with event_callbacks = (id, cb) :: s.event_callbacks }

let push_overlay_event_cb cb id s =
  update_usr_ptr { s with event_callbacks = s.event_callbacks @ [ (id, cb) ] }

let remove_event_cb id s =
  update_usr_ptr
    { s with event_callbacks = List.filter (fun (cb_id, _) -> cb_id <> id) s.event_callbacks }

let is_init state = state.is_init

let current_context state = state.current_context

let swap_interval state = state.swap_interval

let finalise s =
  (match s.current_context with Some w -> Window.destroy w | None -> ());
  ok @@ Glfw.terminate ()

let get_time () = Stubs.Glfw.get_time ()

module Events = Events
