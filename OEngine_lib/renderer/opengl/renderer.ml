open Core.Syntax.Result
open Result

type t = unit

let create () = ()

let clear_color _s r g b a =
  ok @@ Gl.clear_color r g b a

let clear _s =
  ok @@ Gl.clear [ColorBuffer; DepthBuffer]

let draw_indexed _s va =
  let* count = Vertex_array.index_count va in
  Gl.draw_elements Gl.Triangles count
