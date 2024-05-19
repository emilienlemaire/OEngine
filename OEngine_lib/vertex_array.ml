open Core.Syntax.Result
open Result

type t = {
  id : int;
  index : int;
  vertex_buffers: Buffer.Vertex.t list;
  index_buffer: Buffer.Index.t option;
}

let create () =
  let va =
    (fun vas -> List.hd vas) @@ Gl.create_vertex_arrays 1
  in
  { id = va; index = 0; vertex_buffers = []; index_buffer = None }

let bind s =
  let+ _ = Gl.bind_vertex_array s.id in
  s

let unbind s =
  let+ _ = Gl.bind_vertex_array 0 in
  s

let add_vertex_buffer (vb : Buffer.Vertex.t) s =
  let* _ = Gl.bind_vertex_array s.id in
  let* _ = Buffer.Vertex.bind vb in
  let elements = vb.layout.elements in
  let open Buffer.DataType in
  let open Buffer.BufferElement in
  let+ id =
    List.fold_left
      (fun acc element ->
        let* id = acc in
        let+ id =
          match element.typ with
          | Float | Float2 | Float3 | Float4 ->
              let* _ = Gl.enable_vertex_attrib_array id in
              let+ _ =
                Gl.vertex_attrib_pointer id
                  (element_count element.typ)
                  Gl.Float element.normalized vb.layout.stride element.offset
              in
              id + 1
          | Int | Int2 | Int3 | Int4 ->
              let* _ = Gl.enable_vertex_attrib_array id in
              let+ _ =
                Gl.vertex_attrib_pointer id
                  (element_count element.typ)
                  Gl.Int element.normalized vb.layout.stride element.offset
              in
              id + 1
          | Bool ->
              let* _ = Gl.enable_vertex_attrib_array id in
              let+ _ =
                Gl.vertex_attrib_pointer id
                  (element_count element.typ)
                  Gl.Bool element.normalized vb.layout.stride element.offset
              in
              id + 1
          | Mat3 | Mat4 ->
              let count = element_count element.typ in
              let arr = Array.init count Fun.id in
              let id =
                Array.fold_left
                  (fun id i ->
                    let* id = id in
                    let* _ = Gl.enable_vertex_attrib_array id in
                    let+ _ =
                      Gl.vertex_attrib_pointer id count Gl.Float
                        element.normalized vb.layout.stride
                        (element.offset + (Ctypes.(sizeof float) * count * i))
                    in
                    id + 1)
                  (ok id) arr
              in
              id
        in
        id)
      (ok s.index) elements
  in
  { s with index = id; vertex_buffers = vb::s.vertex_buffers }

let index_buffer ib s =
  let* _ = Gl.bind_vertex_array s.id in
  let+ ib = Buffer.Index.bind ib in
  { s with index_buffer = Some ib }

