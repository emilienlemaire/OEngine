open Core.Syntax.Result
open Result
open OEngine_lib

module Layer = OEngine_lib.Layer.Make (struct
  type t = { gl : Gl.t; shader : Shader.t }

  let vertex_shader_source =
    "#version 460\n\
     layout (location = 0) in vec3 aPos;\n\
     void main()\n\
     {\n\
    \  gl_Position = vec4(aPos.x, aPos.y, aPos.z, 1.0);\n\
     }\n"

  let fragment_shader_source =
    "\n\
     #version 460\n\
     out vec4 FragColor;\n\
     void main()\n\
     {\n\
    \   FragColor = vec4(1.0f, 0.5f, 0.2f, 1.0f);\n\
     }\n"

  let create () =
    let gl = Gl.make () in
    let shader = Shader.create gl in
    { gl; shader }

  let attach s =
    let* shader =
      Shader.(add_shader Vertex vertex_shader_source s.shader)
      >>= Shader.(add_shader Fragment fragment_shader_source)
      >>= Shader.create_program
    in
    let vertices = [ -0.5; -0.5; 0.0; 0.5; -0.5; 0.0; 0.0; 0.5; 0.0 ] in
    let vao, gl =
      (fun (vaos, gl) -> (List.hd vaos, gl)) @@ Gl.gen_vertex_arrays 1 s.gl
    in
    let vbo, gl =
      (fun (buffers, gl) -> (List.hd buffers, gl)) @@ Gl.gen_buffers 1 gl
    in
    let* gl = Gl.bind_vertex_array vao gl >>= Gl.bind_buffer ArrayBuffer vbo in
    let* _ = Gl.buffer_data ArrayBuffer vertices StaticDraw gl in
    let* _ = Gl.vertex_attrib_pointer 0 3 Float false 0 0 in
    let* _ = Gl.enable_vertex_attrib_array 0 gl in
    let* gl = Gl.bind_buffer ArrayBuffer 0 gl >>= Gl.bind_vertex_array 0 in
    ok { gl; shader }

  let update s =
    let* _ = Shader.bind s.shader in
    let vao = List.hd @@ Gl.get_vaos s.gl in
    let* gl = Gl.bind_vertex_array vao s.gl in
    let* _ = Gl.draw_arrays Triangles 0 3 gl in
    ok { s with gl }

  let detach s =
    let* _ = Shader.finalize s.shader in
    let* _ = Gl.finalise s.gl in
    ok ()

  let event e s =
    Format.printf "Layer got event: %a@\n"
      (Glfw.Events.pp_event Glfw.Events.pp)
      e;
    Format.pp_print_flush Format.std_formatter ();
    ok (e, s)
end)

let () =
  let$ app = OEngine_lib.Application.make (module Layer) in
  match OEngine_lib.Application.run app with
  | Ok _ ->
      Format.printf "Application finished normally";
      ()
  | Error err ->
      Format.printf "Application finished with errors:@\n%a"
        Core.Error.pp_error_kind err;
      ()
