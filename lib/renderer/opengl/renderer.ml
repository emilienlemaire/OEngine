open Core.Syntax.Result
open Result
open Math

type scene_data = { view_projection_matrix : (float, Bigarray.float32_elt) Mat4x4.t }

type t =
  { clear_color : float * float * float * float;
    scene_data : scene_data
  }

let create () =
  { clear_color = (0., 0., 0., 0.);
    scene_data = { view_projection_matrix = Mat4x4.zeros Base.Float32_elt }
  }

let set_clear_color r g b a s = ok @@ { s with clear_color = (r, g, b, a) }

let clear s =
  let r, g, b, a = s.clear_color in
  Gl.clear_color r g b a;
  Gl.clear [ ColorBuffer; DepthBuffer ];
  ok @@ s

let begin_scene c s =
  ok
  @@ { s with
       scene_data = { view_projection_matrix = Orthographic_camera.view_projection_matrix c }
     }

let submit va sh s =
  let* _ = Shader.bind sh in
  let* _ = Shader.upload_uniform_mat4 "u_ViewProjection" s.scene_data.view_projection_matrix sh in
  let* _ = Vertex_array.bind va in
  let* count = Vertex_array.index_count va in
  let+ _ = Gl.draw_elements Gl.Triangles count in
  s

module Orthographic_camera = Orthographic_camera
