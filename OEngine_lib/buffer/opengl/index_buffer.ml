open Core.Syntax.Result

type t = { id : int; count: int }

let create indices =
  let buffer =
    (fun buffers -> List.hd buffers) @@ Gl.gen_buffers 1
  in
  let* _ = Gl.bind_buffer ElementArrayBuffer buffer in
  let+ _ = Gl.buffer_data ElementArrayBuffer UInt16 indices StaticDraw in
  { id = buffer; count = List.length indices }

let unbind s =
  let+ _ = Gl.bind_buffer ElementArrayBuffer 0 in
  s

let bind s =
  let+ _ = Gl.bind_buffer ElementArrayBuffer s.id in
  s

let data indices s =
  let* s = bind s in
  let+ _ = Gl.buffer_sub_data ElementArrayBuffer UInt16 indices in
  s

let delete s =
  let+ _ = Gl.delete_buffer s.id in
  ()

let count s =
  s.count

