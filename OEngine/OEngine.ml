open Glfw_stubs
open Glad_stubs

let () =
  let _err_fun =
    Glfw.set_error_callback (fun code descr -> Format.eprintf "Err: %d %s@\n" code descr)
  in
  if Glfw.init () = 0 then
    print_endline "Could not init GLFW"
  else
    begin
      Glfw.window_hint 0x00022002 4;
      Glfw.window_hint 0x00022003 6;
      Glfw.window_hint 0x00022008 0x00032001;
      let window =
        Glfw.create_window
        800
        600
        "First window"
        None
        None
      in
    if Ctypes.is_null window then
      print_endline "Could not create window"
  else
    begin
      Glfw.make_context_current window;
      if Glad.load_gl Glfw.get_proc_address = 0 then
        print_endline "Failed to initialize GLAD"
      else
        begin
          while true do
            ()
          done;
        Glfw.terminate ()
        end
      end
    end
