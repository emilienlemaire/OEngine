open Core.Syntax.Result
open Result

type kind = Vertex | Fragment
type t = { id : int; shaders : (kind * int) list }

let create () : t = { id = 0; shaders = [] }

let add_shader kind src s =
  match kind with
  | Vertex ->
      let sid = Gl.create_shader Gl.Vertex in
      let* _ = Gl.shader_source sid src in
      let* _ = Gl.compile_shader sid in
      ok @@ { s with shaders = (Vertex, sid) :: s.shaders }
  | Fragment ->
      let sid = Gl.create_shader Gl.Fragment in
      let* _ = Gl.shader_source sid src in
      let* _ = Gl.compile_shader sid in
      ok @@ { s with shaders = (Fragment, sid) :: s.shaders }

let create_program s =
  let id = Gl.create_program () in
  let* _ =
    List.fold_left
      (fun acc (_, sid) ->
        let* _acc = acc in
        Gl.attach_shader id sid)
      (ok ()) s.shaders
  in
  let* _ = Gl.link_program id in
  let* _ =
    List.fold_left
      (fun acc (_, sid) ->
        let* _acc = acc in
        Gl.detach_shader id sid)
      (ok ()) s.shaders
  in
  ok { s with id }

let delete_shader id s =
  let shaders = List.filter (fun (_, sid) -> sid <> id) s.shaders in
  let* _ = Gl.delete_shader id in
  ok { s with shaders }

let bind s = Gl.use_program s.id
let unbind _ = Gl.unbind_program ()

let finalize s =
  let* _ =
    List.fold_left
      (fun acc (_, sid) ->
        let* _acc = acc in
        Gl.delete_shader sid)
      (ok ()) s.shaders
  in
  ok { s with shaders = [] }
