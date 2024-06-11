open Result
open Core.Syntax.Result
open Ctypes

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

  module Make : functor (_ : LAYER) -> APPLICATION_LAYER
end = struct
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

    let update ts app =
      let+ s = Layer.update ts app !state in
      state := s

    let detach () = Layer.detach !state

    let event e =
      let+ e, s = Layer.event e !state in
      state := s;
      e
  end
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
end = struct

  type t =
    { glfw : Glfw.t;
      layers : (module Layer.APPLICATION_LAYER) Dequeue.t;
      last_frame_time : float
    }

  let null typ = Ctypes.from_voidp typ Ctypes.null

  let ( .@[] ) = Ctypes.CArray.get

  let ( .@[]<- ) = Ctypes.CArray.set

  module ImGuiLayer = struct
    let show = allocate bool true

    let create glfw =
      assert (
        Imguiml.Bindings.debug_check_version_and_data_layout (Imguiml.Bindings.get_version ())
          (sizeof Imguiml.Types.Io.t) (sizeof Imguiml.Types.Style.t) (sizeof Imguiml.Types.Vec2.t)
          (sizeof Imguiml.Types.Vec4.t) (sizeof Imguiml.Types.DrawVert.t)
          (sizeof Imguiml.Types.draw_idx_t) );
      let _ = Imguiml.Bindings.create_context (null Imguiml.Types.FontAtlas.t) in
      Imguiml.Bindings.style_colors_dark (null Imguiml.Types.Style.t);
      let io = Imguiml.Bindings.get_io () in
      io |-> Imguiml.Types.Io.config_flags
      <-@ !@(io |-> Imguiml.Types.Io.config_flags)
          lor Int64.to_int Imguiml.Types.ConfigFlags.nav_enable_keyboard;
      io |-> Imguiml.Types.Io.config_flags
      <-@ !@(io |-> Imguiml.Types.Io.config_flags)
          lor Int64.to_int Imguiml.Types.ConfigFlags.docking_enable;
      io |-> Imguiml.Types.Io.config_flags
      <-@ !@(io |-> Imguiml.Types.Io.config_flags)
          lor Int64.to_int Imguiml.Types.ConfigFlags.viewports_enable;
      Imguiml.Bindings.style_colors_dark (null Imguiml.Types.Style.t);
      let style = Imguiml.Bindings.get_style () in
      if
        !@(io |-> Imguiml.Types.Io.config_flags)
        land Int64.to_int Imguiml.Types.ConfigFlags.viewports_enable
        <> 0
      then (
        style |-> Imguiml.Types.Style.window_rounding <-@ 0.0;
        let colors = !@(style |-> Imguiml.Types.Style.colors) in
        let color = colors.@[Int64.to_int Imguiml.Types.Col.window_bg] in
        let () = color @. Imguiml.Types.Vec4.w <-@ 1.0 in
        colors.@[Int64.to_int Imguiml.Types.Col.window_bg] <- color;
        style |-> Imguiml.Types.Style.colors <-@ colors );
      let window = Glfw.current_context glfw in
      let* _ =
        match window with
        | Some w ->
          let ptr =
            Glfw.Window.as_ptr w
            |> Ctypes.coerce (ptr Bindings.Types.Glfw.window) (ptr Imguiml_impl.Glfw.window)
          in
          if Imguiml_impl.Glfw.init_for_opengl ptr true then
            ok ()
          else
            error (Core.Error.OEngine (Core.Error.Custom "Failed to init imgui with GLFW"))
        | None -> ok ()
      in
      if Imguiml_impl.OpenGL3.init (Some "#version 460") then
        ok ()
      else
        error (Core.Error.OEngine (Core.Error.Custom "Failed to init imgui"))

    let update app =
      Imguiml_impl.OpenGL3.new_frame ();
      Imguiml_impl.Glfw.new_frame ();
      Imguiml.Bindings.new_frame ();
      let io = Imguiml.Bindings.get_io () in
      let+ width, heigth = Glfw.get_current_context_size app.glfw in
      let size = Imguiml.Bindings.ImVec2.float_ (float_of_int width) (float_of_int heigth) in
      io |-> Imguiml.Types.Io.display_size <-@ !@size;
      if
        !@(io |-> Imguiml.Types.Io.config_flags)
        land Int64.to_int Imguiml.Types.ConfigFlags.viewports_enable
        <> 0
      then (
        let backup_context = Option.get @@ Glfw.current_context app.glfw in
        Imguiml.Bindings.update_platform_windows ();
        Imguiml.Bindings.render_platform_windows_default Ctypes.null Ctypes.null;
        let _ = Glfw.make_current_context backup_context app.glfw in
        () );
      Imguiml.Bindings.show_demo_window show;
      Imguiml.Bindings.render ();
      Imguiml_impl.OpenGL3.render_draw_data (Imguiml.Bindings.get_draw_data ());
      ()

    (* let on_event e =
       let io = Bindings.get_io () in
       match e with Events.KeyPressed e -> () *)
  end

  let key_callback glfw key _scancode action _mods =
    if key = Events.Keys.ESCAPE && action = Events.Actions.PRESS then
      ignore @@ Glfw.set_window_should_close true glfw

  let framebuffer_size_callback _ width height = Stubs.Gl.viewport 0 0 width height

  let make (module FirstLayer : Layer.APPLICATION_LAYER) =
    let* glfw =
      Glfw.make ()
      >>= Glfw.set_error_callback (fun code descr -> Format.eprintf "[Error] %d: %s@\n" code descr)
      >>= Glfw.init
    in
    Glfw.(Window.window_hint Window.Context_version_major 4);
    Glfw.(Window.window_hint Window.Context_version_minor 6);
    Glfw.(Window.window_hint Window.Opengl_profile Window.Core);
    let window = Glfw.Window.make ~width:1600 ~height:900 ~title:"GLFW Window" () in
    let* glfw =
      Glfw.make_current_context window glfw
      >>= Glfw.set_key_callback key_callback
      >>= Glfw.set_framebuffer_size_callback framebuffer_size_callback
      >>= Glew.init >>= Glfw.set_swap_interval 1
    in
    let _ = ImGuiLayer.create glfw in
    let layers = Dequeue.make @@ (module FirstLayer : Layer.APPLICATION_LAYER) in
    let* _ = FirstLayer.attach () in
    let+ glfw = Glfw.push_layer_event_cb FirstLayer.event FirstLayer.id glfw in
    let time = Glfw.get_time () in
    { glfw; layers; last_frame_time = time }

  let update s =
    let time = Glfw.get_time () in
    let time_step = time -. s.last_frame_time in
    let* _ =
      Dequeue.fold_left
        (fun acc (module Layer : Layer.APPLICATION_LAYER) ->
          let* _ = acc in
          Layer.update time_step s )
        (ok ()) s.layers
    in
    let* _ = ImGuiLayer.update s in
    let+ glfw = Glfw.swap_buffers s.glfw >>= Glfw.poll_events in
    { s with glfw; last_frame_time = time }

  let should_stop s = Glfw.window_should_close s.glfw

  let finalise app =
    let* _ =
      Dequeue.fold_left
        (fun acc (module Layer : Layer.APPLICATION_LAYER) ->
          let* _ = acc in
          Layer.detach () )
        (ok ()) app.layers
    in
    Glfw.finalise app.glfw

  let glfw s = s.glfw

  let push_layer (module NewLayer : Layer.APPLICATION_LAYER) s =
    let* glfw = Glfw.push_layer_event_cb NewLayer.event NewLayer.id s.glfw in
    let s =
      { s with glfw; layers = Dequeue.append (module NewLayer : Layer.APPLICATION_LAYER) s.layers }
    in
    let+ _ = NewLayer.attach () in
    s

  let push_overlay (module NewLayer : Layer.APPLICATION_LAYER) s =
    let* glfw = Glfw.push_overlay_event_cb NewLayer.event NewLayer.id s.glfw in
    let s =
      { s with glfw; layers = Dequeue.prepend (module NewLayer : Layer.APPLICATION_LAYER) s.layers }
    in
    let+ _ = NewLayer.attach () in
    s

  let remove_layer (module Removed : Layer.APPLICATION_LAYER) s =
    let* glfw = Glfw.remove_event_cb Removed.id s.glfw in
    let+ () = Removed.detach () in
    { s with glfw; layers = Dequeue.remove (module Removed : Layer.APPLICATION_LAYER) s.layers }

  let rec run app =
    let* app = update app in
    let* close = should_stop app in
    if close then
      ok app
    else
      run app
end
