open Core.Syntax.Result
open Result

type t = { clear_color : float * float * float * float }

let create () = { clear_color = (0., 0., 0., 0.) }
let set_clear_color r g b a _s = ok @@ { clear_color = (r, g, b, a) }

let clear s =
  let r, g, b, a = s.clear_color in
  Gl.clear_color r g b a;
  Gl.clear [ ColorBuffer; DepthBuffer ];
  ok @@ s

let submit s va =
  let* _ = Vertex_array.bind va in
  let* count = Vertex_array.index_count va in
  let+ _ = Gl.draw_elements Gl.Triangles count in
  s
