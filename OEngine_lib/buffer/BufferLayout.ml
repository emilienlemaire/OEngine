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
