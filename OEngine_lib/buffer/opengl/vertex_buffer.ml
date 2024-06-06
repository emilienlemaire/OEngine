open Core.Syntax.Result

type t = { id : int; layout : BufferLayout.t }

let init vertices layout =
  let buffer = (fun buffers -> List.hd buffers) @@ Gl.gen_buffers 1 in
  let* _ = Gl.bind_buffer ArrayBuffer buffer in
  let+ _ = Gl.buffer_data ArrayBuffer Float vertices DynamicDraw in
  { id = buffer; layout }

let create size layout =
  let buffer = (fun buffers -> List.hd buffers) @@ Gl.gen_buffers 1 in
  let* _ = Gl.bind_buffer ArrayBuffer buffer in
  let+ _ = Gl.buffer_data ArrayBuffer (None size) [] DynamicDraw in
  { id = buffer; layout }

let unbind s =
  let+ _ = Gl.bind_buffer ArrayBuffer 0 in
  s

let bind s =
  let+ _ = Gl.bind_buffer ArrayBuffer s.id in
  s

let data vertices s =
  let* s = bind s in
  let+ _ = Gl.buffer_sub_data ArrayBuffer Float vertices in
  s

let layout s = s.layout

let delete s =
  let+ _ = Gl.delete_buffer s.id in
  ()
