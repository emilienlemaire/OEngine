open Core.Syntax.Result

(* TODO: - layout *)

module DataType = struct
  type t =
    | Float
    | Float2
    | Float3
    | Float4
    | Mat3
    | Mat4
    | Int
    | Int2
    | Int3
    | Int4
    | Bool

  let size_in_bytes = function
    | Float | Int -> 4
    | Float2 | Int2 -> 4 * 2
    | Float3 | Int3 -> 4 * 3
    | Float4 | Int4 -> 4 * 4
    | Mat3 -> 4 * 3 * 3
    | Mat4 -> 4 * 4 * 4
    | Bool -> 1

  let element_count = function
    | Float2 | Int2 -> 2
    | Float | Int | Bool -> 1
    | Float3 | Int3 | Mat3 -> 3
    | Float4 | Int4 | Mat4 -> 4
end

module BufferElement = struct
  type t = {
    name : string;
    typ : DataType.t;
    size : int;
    offset : int;
    normalized : bool;
  }

  let create ?(offset = 0) ?(normalized = false) name typ size =
    { name; typ; size; offset; normalized }
end

module BufferLayout = struct
  type t = { elements : BufferElement.t list; stride : int }

  let create elements =
    let stride, elements =
      List.fold_left
        (fun (offset, elements) (name, data_type) ->
          let elt =
            BufferElement.create name data_type
              (DataType.size_in_bytes data_type)
              ~offset
          in
          (offset + DataType.size_in_bytes data_type, elt :: elements))
        (0, []) elements
    in
    { elements = List.rev elements; stride }

  let add element_name element_type s =
    let element =
      BufferElement.create element_name element_type
        (DataType.size_in_bytes element_type)
        ~offset:s.stride
    in
    {
      elements = s.elements @ [ element ];
      stride = s.stride + DataType.size_in_bytes element_type;
    }

end

module Vertex = struct
  type t = { id : int; layout : BufferLayout.t }

  let init vertices layout =
    let buffer =
      (fun buffers -> List.hd buffers) @@ Gl.gen_buffers 1
    in
    let* _ = Gl.bind_buffer ArrayBuffer buffer in
    let+ _ = Gl.buffer_data ArrayBuffer Float vertices DynamicDraw in
    { id = buffer; layout }

  let create size layout =
    let buffer =
      (fun buffers -> List.hd buffers) @@ Gl.gen_buffers 1
    in
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

  let delete s =
    let+ _ = Gl.delete_buffer s.id in
    ()
end

module Index = struct
  type t = { id : int; }

  let create indices =
    let buffer =
      (fun buffers -> List.hd buffers) @@ Gl.gen_buffers 1
    in
    let* _ = Gl.bind_buffer ElementArrayBuffer buffer in
    let+ _ = Gl.buffer_data ElementArrayBuffer UInt16 indices StaticDraw in
    { id = buffer }

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
end
