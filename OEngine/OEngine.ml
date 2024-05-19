open Core.Syntax.Result
open Result
open OEngine_lib

module Layer = OEngine_lib.Layer.Make (struct
  type t = {
    shader : Shader.t;
    blue_shader : Shader.t;
    vertex_array : Vertex_array.t option;
    vertex_buffer : Buffer.Vertex.t option; [@warning "-69"]
    index_buffer : Buffer.Index.t option; [@warning "-69"]
    square_vertex_array : Vertex_array.t option;
  }

  let vertex_shader_source =
    {|
#version 460
layout (location = 0) in vec3 a_Position;
layout (location = 1) in vec4 a_Color;

out vec3 v_Position;
out vec4 v_Color;

void main()
{
  v_Position = a_Position;
  v_Color = a_Color;
  gl_Position = vec4(a_Position, 1.0);
}|}

  let fragment_shader_source =
    {|
#version 460

layout (location = 0) out vec4 color;

in vec3 v_Position;
in vec4 v_Color;

void main()
{
  color = vec4(v_Position * 0.5 + 0.5, 1.0);
  color = v_Color;
}|}

  let blue_vertex_source =
    {|
#version 460

layout (location = 0) in vec3 a_Position;

out vec3 v_Position;

void main() {
  v_Position = a_Position;
  gl_Position = vec4(a_Position, 1.0);
}
    |}

  let blue_fragment_source =
    {|
#version 460

layout (location = 0) out vec4 color;

in vec3 v_Color;

void main() {
  color = vec4(0.2, 0.3, 0.8, 1.0);
}
    |}

  let create () =
    let shader = Shader.create () in
    let blue_shader = Shader.create () in
    {
      shader;
      blue_shader;
      vertex_buffer = None;
      index_buffer = None;
      vertex_array = None;
      square_vertex_array = None;
    }

  let attach s =
    let vertex_array = Vertex_array.create () in
    let vertices =
      [ -0.5; -0.5; 0.0; 0.8; 0.2; 0.8; 1.0;
         0.5; -0.5; 0.0; 0.2; 0.3; 0.8; 1.0;
         0.0;  0.5; 0.0; 0.8; 0.8; 0.2; 1.0 ] [@ocamlformat "disable"]
    in
    let layout =
      Buffer.BufferLayout.create
        [
          ("a_Position", Buffer.DataType.Float3);
          ("a_Color", Buffer.DataType.Float4);
        ]
    in
    let* vertex_buffer = Buffer.Vertex.init vertices layout in
    let* vertex_array =
      Vertex_array.add_vertex_buffer vertex_buffer vertex_array
    in
    let* index_buffer = Buffer.Index.create [ 0; 1; 2 ] in
    let* vertex_buffer = Buffer.Vertex.unbind vertex_buffer in
    let square_vertex_array = Vertex_array.create () in
    let square_vertices =
      [ -0.75; -0.75; 0.0;
         0.75; -0.75; 0.0;
         0.75;  0.75; 0.0;
        -0.75;  0.75; 0.0 ] [@ocamlformat "disable"]
    in
    let square_layout =
      Buffer.BufferLayout.create [ ("a_Position", Buffer.DataType.Float3) ]
    in
    let* square_vertex_buffer =
      Buffer.Vertex.init square_vertices square_layout
    in
    let* square_vertex_array =
      Vertex_array.add_vertex_buffer square_vertex_buffer square_vertex_array
    in
    let square_indices = [ 0; 1; 2; 2; 3; 0 ] in
    let* square_index_buffer = Buffer.Index.create square_indices in
    let* square_vertex_array =
      Vertex_array.index_buffer square_index_buffer square_vertex_array
    in
    let* shader =
      Shader.(add_shader Vertex vertex_shader_source s.shader)
      >>= Shader.(add_shader Fragment fragment_shader_source)
      >>= Shader.create_program
    in
    let* blue_shader =
      Shader.(add_shader Vertex blue_vertex_source s.blue_shader)
      >>= Shader.(add_shader Fragment blue_fragment_source)
      >>= Shader.create_program
    in
    ok
      {
        shader;
        blue_shader;
        vertex_buffer = Some vertex_buffer;
        index_buffer = Some index_buffer;
        vertex_array = Some vertex_array;
        square_vertex_array = Some square_vertex_array;
      }

  let update s =
    Stubs.Gl.clear_color 0.1 0.1 0.1 1.0;
    Stubs.Gl.clear Stubs.Gl.color_buffer_bit;
    let* _ = Shader.bind s.blue_shader in
    let* _ = Vertex_array.bind (Option.get s.square_vertex_array) in
    let* _ = Gl.draw_elements Gl.Triangles 6 in
    let* _ = Shader.bind s.shader in
    let* _ = Vertex_array.bind (Option.get s.vertex_array) in
    let* _ = Gl.draw_elements Gl.Triangles 3 in
    ok s

  let detach s =
    let* _ = Shader.finalize s.shader in
    let* _ = Gl.finalise () in
    ok ()

  let event e s =
    Format.printf "Layer got event: %a@\n"
      (Glfw.Events.pp_event Glfw.Events.pp)
      e;
    Format.pp_print_flush Format.std_formatter ();
    ok (e, s)
end)

let () =
  let app = OEngine_lib.Application.make (module Layer) in
  match app with
  | Ok app ->
    (match OEngine_lib.Application.run app with
    | Ok _ ->
        Format.printf "Application finished normally";
        ()
    | Error err ->
        Format.printf "Application finished with errors:@\n%a"
          Core.Error.pp_error_kind err;
        ())
    | Error err ->
        Format.printf "Application could not be created:@\n%a"
        Core.Error.pp_error_kind err
