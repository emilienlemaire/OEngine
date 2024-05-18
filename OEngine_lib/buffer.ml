open Core.Syntax.Result

type t = {
  buffers: int list;
  gl : Gl.t;
}

(* TODO: - Bind
         - Data
         - Delete *)

let create gl: t =
  { buffers = []; gl }

let create_vertex_buffer vertices s =
  let buffer, gl =
    (fun (buffers, gl) -> (List.hd buffers, gl)) @@ Gl.gen_buffers 1 s.gl
  in
  let* gl = Gl.bind_buffer ArrayBuffer buffer gl in
  let+ _ = Gl.buffer_data ArrayBuffer Float vertices DynamicDraw gl in
  { buffers = buffer::s.buffers; gl }

let unbind_vertex_buffer s =
  let+ gl = Gl.bind_buffer ArrayBuffer 0 s.gl in
  { s with gl = gl }
