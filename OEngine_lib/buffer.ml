open Core.Syntax.Result

(* TODO: - layout *)

module Vertex = struct
  type t = { id : int; gl : Gl.t }

  let create gl vertices =
    let buffer, gl =
      (fun (buffers, gl) -> (List.hd buffers, gl)) @@ Gl.gen_buffers 1 gl
    in
    let* gl = Gl.bind_buffer ArrayBuffer buffer gl in
    let+ _ = Gl.buffer_data ArrayBuffer Float vertices DynamicDraw gl in
    { id = buffer; gl }

  let unbind s =
    let+ gl = Gl.bind_buffer ArrayBuffer 0 s.gl in
    { s with gl }

  let bind s =
    let+ gl = Gl.bind_buffer ArrayBuffer s.id s.gl in
    { s with gl }

  let data vertices s =
    let* s = bind s in
    Gl.buffer_sub_data ArrayBuffer Float vertices s.gl

  let delete s =
    let+ gl = Gl.delete_buffer s.id s.gl in
    gl
end

module Index = struct
  type t = { id : int; gl : Gl.t }

  let create gl indices =
    let buffer, gl =
      (fun (buffers, gl) -> (List.hd buffers, gl)) @@ Gl.gen_buffers 1 gl
    in
    let* gl = Gl.bind_buffer ElementArrayBuffer buffer gl in
    let+ _ = Gl.buffer_data ElementArrayBuffer UInt16 indices StaticDraw gl in
    { id = buffer; gl }

  let unbind s =
    let+ gl = Gl.bind_buffer ElementArrayBuffer 0 s.gl in
    { s with gl }

  let bind s =
    let+ gl = Gl.bind_buffer ElementArrayBuffer s.id s.gl in
    { s with gl }

  let data indices s =
    let* s = bind s in
    Gl.buffer_sub_data ElementArrayBuffer UInt16 indices s.gl

  let delete s =
    let+ gl = Gl.delete_buffer s.id s.gl in
    gl
end
