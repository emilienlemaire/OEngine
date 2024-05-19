open Result
open Core.Syntax.Result

type t = { glfw : Glfw.t; layers : (module Layer.APPLICATION_LAYER) Dequeue.t }

let key_callback glfw key _scancode action _mods =
  if key = Events.Keys.ESCAPE && action = Events.Actions.PRESS then
    ignore @@ Glfw.set_window_should_close true glfw
  else if key = Events.Keys.Q && action = Events.Actions.RELEASE then (
    Format.printf "You released the Q key@\n";
    Format.pp_print_flush Format.std_formatter ())
  else if key = Events.Keys.Q && action = Events.Actions.REPEAT then (
    Format.printf "You repeated the Q key@\n";
    Format.pp_print_flush Format.std_formatter ())
  else
    ()

let framebuffer_size_callback _ width height =
  Stubs.Gl.viewport 0 0 width height

let make (module FirstLayer : Layer.APPLICATION_LAYER) =
  let* glfw =
    Glfw.make ()
    >>= Glfw.set_error_callback (fun code descr ->
            Format.eprintf "[Error] %d: %s@\n" code descr)
    >>= Glfw.init
  in
  Glfw.(Window.window_hint Window.Context_version_major 4);
  Glfw.(Window.window_hint Window.Context_version_minor 6);
  Glfw.(Window.window_hint Window.Opengl_profile Window.Core);
  let window =
    Glfw.Window.make ~width:640 ~height:480 ~title:"GLFW Window" ()
  in
  let* glfw =
    Glfw.make_current_context window glfw
    >>= Glfw.set_key_callback key_callback
    >>= Glfw.set_framebuffer_size_callback framebuffer_size_callback
    >>= Glew.init >>= Glfw.set_swap_interval 1
  in
  let layers = Dequeue.make @@ (module FirstLayer : Layer.APPLICATION_LAYER) in
  let* _ = FirstLayer.attach () in
  let+ glfw = Glfw.push_layer_event_cb FirstLayer.event FirstLayer.id glfw in
  { glfw; layers }

let update s =
  Stubs.Gl.clear_color 0.2 0.3 0.3 1.0;
  Stubs.Gl.clear (Stubs.Gl.color_buffer_bit lor Stubs.Gl.depth_buffer_bit);
  let* _ =
    Dequeue.fold_left
      (fun acc (module Layer : Layer.APPLICATION_LAYER) ->
        let* _ = acc in
        Layer.update ())
      (ok ()) s.layers
  in
  let* glfw = Glfw.swap_buffers s.glfw >>= Glfw.poll_events in
  ok { s with glfw }

(* let stop s =
   let+ glfw = Glfw.set_window_should_close true s.glfw in
   { s with glfw } *)

let should_stop s = Glfw.window_should_close s.glfw

let finalise app =
  let* _ =
    Dequeue.fold_left
      (fun acc (module Layer : Layer.APPLICATION_LAYER) ->
        let* _ = acc in
        Layer.detach ())
      (ok ()) app.layers
  in
  Glfw.finalise app.glfw

let push_layer (module NewLayer : Layer.APPLICATION_LAYER) s =
  let* glfw = Glfw.push_layer_event_cb NewLayer.event NewLayer.id s.glfw in
  let s =
    {
      glfw;
      layers =
        Dequeue.append (module NewLayer : Layer.APPLICATION_LAYER) s.layers;
    }
  in
  let+ _ = NewLayer.attach () in
  s

let push_overlay (module NewLayer : Layer.APPLICATION_LAYER) s =
  let* glfw = Glfw.push_overlay_event_cb NewLayer.event NewLayer.id s.glfw in
  let s =
    {
      glfw;
      layers =
        Dequeue.prepend (module NewLayer : Layer.APPLICATION_LAYER) s.layers;
    }
  in
  let+ _ = NewLayer.attach () in
  s

let remove_layer (module Removed : Layer.APPLICATION_LAYER) s =
  let* glfw = Glfw.remove_event_cb Removed.id s.glfw in
  let+ () = Removed.detach () in
  {
    glfw;
    layers = Dequeue.remove (module Removed : Layer.APPLICATION_LAYER) s.layers;
  }

let rec run app =
  let* app = update app in
  let* close = should_stop app in
  if close then
    ok app
  else
    run app
