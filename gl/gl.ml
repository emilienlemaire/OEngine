open Ctypes
open Result
open Core.Error
open Core.Syntax.Result

type t = {
  binded_vao : int option;
  binded_vbo : int option;
  vaos : int list;
  buffers : int list;
  pid : int option;
}

let bigarray_create k len = Bigarray.(Array1.create k c_layout len)

let string_of_bigarray ba =
  let len = Bigarray.Array1.dim ba in
  let b = Buffer.create (len - 1) in
  try
    for i = 0 to len - 1 do
      if ba.{i} = '\x00' then
        raise Exit
      else
        Buffer.add_char b ba.{i}
    done;
    raise Exit
  with Exit -> Buffer.contents b

let get_int =
  let a = bigarray_create Bigarray.int32 1 in
  fun f ->
    f a;
    Int32.to_int a.{0}

(* let set_int =
   let a = bigarray_create Bigarray.int32 1 in
   fun f i -> a.{0} <- Int32.of_int i; f a *)

let get_string_ len f =
  let a = bigarray_create Bigarray.char len in
  f a;
  string_of_bigarray a

type shader_type =
  | Vertex
  | Geometry
  | Fragment
  | Compute
  | TessControl
  | TessEvaluation

type _ shader_parameter =
  | ShaderType : shader_type shader_parameter
  | DeleteStatus : bool shader_parameter
  | CompileStatus : bool shader_parameter
  | InfoLogLength : int shader_parameter
  | ShaderSourceLength : int shader_parameter
  | SPIRVBinary : bool shader_parameter

type string_ = Vendor | Renderer | Version | ShadingLanguageVersion

let make () =
  { binded_vao = None; vaos = []; binded_vbo = None; buffers = []; pid = None }

let check_error f =
  let res = f () in
  match Stubs.Gl.get_error () with
  | x when x = Stubs.Gl.no_error -> ok res
  | x when x = Stubs.Gl.invalid_enum -> error (Gl Invalid_enum)
  | x when x = Stubs.Gl.invalid_value -> error (Gl Invalid_value)
  | x when x = Stubs.Gl.invalid_operation -> error (Gl Invalid_operation)
  | x when x = Stubs.Gl.invalid_framebuffer_operation ->
      error (Gl Invalid_framebuffer_operation)
  | x when x = Stubs.Gl.out_of_memory -> error (Gl Out_of_memory)
  | x when x = Stubs.Gl.stack_underflow -> error (Gl Stack_underflow)
  | x when x = Stubs.Gl.stack_overflow -> error (Gl Stack_overflow)
  | _ -> assert false

let get_string s _ =
  ok @@ Stubs.Gl.get_string
  @@
  match s with
  | Vendor -> Stubs.Gl.vendor
  | Renderer -> Stubs.Gl.renderer
  | Version -> Stubs.Gl.version
  | ShadingLanguageVersion -> Stubs.Gl.shading_language_version

let get_shaderiv : type a. int -> a shader_parameter -> a =
 fun sid -> function
  | ShaderType ->
      (match get_int (Stubs.Gl.get_shaderiv sid Stubs.Gl.shader_type) with
      | x when x = Stubs.Gl.vertex_shader -> Vertex
      | x when x = Stubs.Gl.fragment_shader -> Fragment
      | x when x = Stubs.Gl.geometry_shader -> Geometry
      | x when x = Stubs.Gl.compute_shader -> Compute
      | x when x = Stubs.Gl.tess_control_shader -> TessControl
      | x when x = Stubs.Gl.tess_evaluation_shader -> TessEvaluation
      | _ -> assert false)
  | DeleteStatus ->
      get_int (Stubs.Gl.get_shaderiv sid Stubs.Gl.delete_status)
      = Stubs.Gl.true_
  | CompileStatus ->
      get_int (Stubs.Gl.get_shaderiv sid Stubs.Gl.compile_status)
      = Stubs.Gl.true_
  | InfoLogLength ->
      get_int (Stubs.Gl.get_shaderiv sid Stubs.Gl.info_log_length)
  | ShaderSourceLength ->
      get_int (Stubs.Gl.get_shaderiv sid Stubs.Gl.shader_source_length)
  | SPIRVBinary ->
      get_int (Stubs.Gl.get_shaderiv sid Stubs.Gl.spir_v_binary)
      = Stubs.Gl.true_

let create_shader : shader_type -> int = function
  | Vertex -> Stubs.Gl.create_shader Stubs.Gl.vertex_shader
  | Fragment -> Stubs.Gl.create_shader Stubs.Gl.fragment_shader
  | Geometry -> Stubs.Gl.create_shader Stubs.Gl.geometry_shader
  | Compute -> Stubs.Gl.create_shader Stubs.Gl.compute_shader
  | TessControl -> Stubs.Gl.create_shader Stubs.Gl.tess_control_shader
  | TessEvaluation -> Stubs.Gl.create_shader Stubs.Gl.tess_evaluation_shader

let shader_source sid src =
  check_error (fun () ->
      let str = allocate string src in
      Stubs.Gl.shader_source sid 1 str None)

let get_shader_source sid =
  let len = get_shaderiv sid ShaderSourceLength in
  get_string_ len (Stubs.Gl.get_shader_source sid len None)

let shader_info_log sid =
  let len = get_shaderiv sid InfoLogLength in
  get_string_ len (Stubs.Gl.get_shader_info_log sid len None)

let compile_shader sid =
  let* _ = check_error (fun () -> Stubs.Gl.compile_shader sid) in
  if get_shaderiv sid CompileStatus then
    ok ()
  else
    let log = shader_info_log sid in
    error (Gl (ShaderCompilationFailed log))

type buffer_mode =
  (* NOTE: Maybe a better name *)
  | SeparateAttribs
  | InterleavedAttribs

type geometry_input_type =
  | Points
  | Lines
  | LinesAdjacency
  | Triangles
  | TrianglesAdjacency

type geometry_output_type = Points | LineStrip | TriangleStrip
type tess_mode = Quads | Triangles | Isolines
type tess_spacing = Equal | FractionalEven | FractionalOdd
type tess_vertex_order = CCW | CW

type _ program_parameter =
  | DeleteStatus : bool program_parameter
  | LinkStatus : bool program_parameter
  | ValidateStatus : bool program_parameter
  | InfoLogLength : int program_parameter
  | AttachedShaders : int program_parameter
  | ActiveAtomicCounterBuffers : int program_parameter
  | ActiveAttributes : int program_parameter
  | ActiveAttributesMaxLength : int program_parameter
  | ActiveUniforms : int program_parameter
  | ActiveUniformBlocks : int program_parameter
  | ActiveUniformBlockMaxNameLength : int program_parameter
  | ActiveUniformMaxLength : int program_parameter
  | ComputeWorkGroupSize : int program_parameter
  | ProgramBinaryLength : int program_parameter
  | TransformFeedbackBufferMode : buffer_mode program_parameter
  | TransformFeedbackVaryings : int program_parameter
  | TransformFeedbackVaryingMaxLength : int program_parameter
  | GeometryVerticesOut : int program_parameter
  | GeometryInputType : geometry_input_type program_parameter
  | GeometryOutputType : geometry_output_type program_parameter
  | GeometryShaderInvocations : int program_parameter
  | TessControlOuputVerticies : int program_parameter
  | TessGenMode : tess_mode program_parameter
  | TessGenSpacing : tess_spacing program_parameter
  | TessGenVertexOrder : tess_vertex_order program_parameter
  | TessGenPointMode : bool program_parameter
  | ProgramSeparable : bool program_parameter
  | ProgramBinaryRetrievableHint : bool program_parameter

let create_program s =
  let pid = Stubs.Gl.create_program () in
  (pid, { s with pid = Some pid })

let attach_shader pid shader =
  check_error (fun () -> Stubs.Gl.attach_shader pid shader)

let get_programiv : type a. int -> a program_parameter -> a =
 fun pid -> function
  | DeleteStatus ->
      get_int (Stubs.Gl.get_programiv pid Stubs.Gl.delete_status)
      = Stubs.Gl.true_
  | LinkStatus ->
      get_int (Stubs.Gl.get_programiv pid Stubs.Gl.link_status) = Stubs.Gl.true_
  | ValidateStatus ->
      get_int (Stubs.Gl.get_programiv pid Stubs.Gl.validate_status)
      = Stubs.Gl.true_
  | InfoLogLength ->
      get_int (Stubs.Gl.get_programiv pid Stubs.Gl.info_log_length)
  | AttachedShaders ->
      get_int (Stubs.Gl.get_programiv pid Stubs.Gl.attached_shaders)
  | ActiveAtomicCounterBuffers ->
      get_int
        (Stubs.Gl.get_programiv pid Stubs.Gl.active_atomic_counter_buffers)
  | ActiveAttributes ->
      get_int (Stubs.Gl.get_programiv pid Stubs.Gl.active_attributes)
  | ActiveAttributesMaxLength ->
      get_int (Stubs.Gl.get_programiv pid Stubs.Gl.active_attribute_max_length)
  | ActiveUniforms ->
      get_int (Stubs.Gl.get_programiv pid Stubs.Gl.active_uniforms)
  | ActiveUniformBlocks ->
      get_int (Stubs.Gl.get_programiv pid Stubs.Gl.active_uniform_blocks)
  | ActiveUniformBlockMaxNameLength ->
      get_int
        (Stubs.Gl.get_programiv pid
           Stubs.Gl.active_uniform_block_max_name_length)
  | ActiveUniformMaxLength ->
      get_int (Stubs.Gl.get_programiv pid Stubs.Gl.active_uniform_max_length)
  | ComputeWorkGroupSize ->
      get_int (Stubs.Gl.get_programiv pid Stubs.Gl.compute_work_group_size)
  | ProgramBinaryLength ->
      get_int (Stubs.Gl.get_programiv pid Stubs.Gl.program_binary_length)
  | TransformFeedbackBufferMode ->
      (match
         get_int
           (Stubs.Gl.get_programiv pid Stubs.Gl.transform_feedback_buffer_mode)
       with
      | x when x = Stubs.Gl.separate_attribs -> SeparateAttribs
      | x when x = Stubs.Gl.interleaved_attribs -> InterleavedAttribs
      | _ -> assert false)
  | TransformFeedbackVaryings ->
      get_int (Stubs.Gl.get_programiv pid Stubs.Gl.transform_feedback_varyings)
  | TransformFeedbackVaryingMaxLength ->
      get_int
        (Stubs.Gl.get_programiv pid
           Stubs.Gl.transform_feedback_varying_max_length)
  | GeometryVerticesOut ->
      get_int (Stubs.Gl.get_programiv pid Stubs.Gl.geometry_vertices_out)
  | GeometryInputType ->
      (match
         get_int (Stubs.Gl.get_programiv pid Stubs.Gl.geometry_input_type)
       with
      | x when x = Stubs.Gl.points -> Points
      | x when x = Stubs.Gl.lines -> Lines
      | x when x = Stubs.Gl.lines_adjacency -> LinesAdjacency
      | x when x = Stubs.Gl.triangles -> Triangles
      | x when x = Stubs.Gl.triangles_adjacency -> TrianglesAdjacency
      | _ -> assert false)
  | GeometryOutputType ->
      (match
         get_int (Stubs.Gl.get_programiv pid Stubs.Gl.geometry_output_type)
       with
      | x when x = Stubs.Gl.points -> Points
      | x when x = Stubs.Gl.line_strip -> LineStrip
      | x when x = Stubs.Gl.triangle_strip -> TriangleStrip
      | _ -> assert false)
  | GeometryShaderInvocations ->
      get_int (Stubs.Gl.get_programiv pid Stubs.Gl.geometry_shader_invocations)
  | TessControlOuputVerticies ->
      get_int (Stubs.Gl.get_programiv pid Stubs.Gl.tess_control_output_vertices)
  | TessGenMode ->
      (match get_int (Stubs.Gl.get_programiv pid Stubs.Gl.tess_gen_mode) with
      | x when x = Stubs.Gl.quads -> Quads
      | x when x = Stubs.Gl.triangles -> Triangles
      | x when x = Stubs.Gl.isolines -> Isolines
      | _ -> assert false)
  | TessGenSpacing ->
      (match get_int (Stubs.Gl.get_programiv pid Stubs.Gl.tess_gen_spacing) with
      | x when x = Stubs.Gl.equal -> Equal
      | x when x = Stubs.Gl.fractional_odd -> FractionalOdd
      | x when x = Stubs.Gl.fractional_even -> FractionalEven
      | _ -> assert false)
  | TessGenVertexOrder ->
      (match
         get_int (Stubs.Gl.get_programiv pid Stubs.Gl.tess_gen_vertex_order)
       with
      | x when x = Stubs.Gl.cw -> CW
      | x when x = Stubs.Gl.ccw -> CCW
      | _ -> assert false)
  | TessGenPointMode ->
      get_int (Stubs.Gl.get_programiv pid Stubs.Gl.tess_gen_point_mode)
      = Stubs.Gl.true_
  | ProgramSeparable ->
      get_int (Stubs.Gl.get_programiv pid Stubs.Gl.program_separable)
      = Stubs.Gl.true_
  | ProgramBinaryRetrievableHint ->
      get_int
        (Stubs.Gl.get_programiv pid Stubs.Gl.program_binary_retrievable_hint)
      = Stubs.Gl.true_

let program_info_log sid =
  let len = get_shaderiv sid InfoLogLength in
  get_string_ len (Stubs.Gl.get_shader_info_log sid len None)

let link_program pid =
  let* _ = check_error (fun () -> Stubs.Gl.link_program pid) in
  if get_programiv pid LinkStatus then
    ok ()
  else
    let log = program_info_log pid in
    error (Gl (ProgramLinkingFailed log))

let detach_shader pid sid =
  check_error (fun () -> Stubs.Gl.detach_shader pid sid)

let delete_shader sid = check_error (fun () -> Stubs.Gl.delete_shader sid)

let gen_vertex_arrays n state =
  let a = bigarray_create Bigarray.int32 n in
  Stubs.Gl.gen_vertex_arrays n a;
  let res = Array.make n 0 in
  for i = 0 to n - 1 do
    res.(i) <- Int32.to_int a.{i}
  done;
  let new_state = { state with vaos = state.vaos @ Array.to_list res } in
  (Array.to_list res, new_state)

let gen_buffers n state =
  let a = bigarray_create Bigarray.int32 n in
  Stubs.Gl.gen_buffers n a;
  let res = Array.make n 0 in
  for i = 0 to n - 1 do
    res.(i) <- Int32.to_int a.{i}
  done;
  let new_state = { state with buffers = state.buffers @ Array.to_list res } in
  (Array.to_list res, new_state)

type buffer_target = ArrayBuffer

let bind_vertex_array vao s =
  if vao = 0 then (
    Stubs.Gl.bind_vertex_array vao;
    ok { s with binded_vao = None })
  else
    match List.find_opt (( = ) vao) s.vaos with
    | None -> error (Gl (UnknownVertexArray vao))
    | Some vao ->
        Stubs.Gl.bind_vertex_array vao;
        ok { s with binded_vao = Some vao }

let bind_buffer typ buffer s =
  if buffer = 0 then (
    match typ with
    | ArrayBuffer ->
        Stubs.Gl.bind_buffer Stubs.Gl.array_buffer buffer;
        ok { s with binded_vbo = None })
  else
    match List.find_opt (( = ) buffer) s.buffers with
    | None -> error (Gl (UnknownBuffer buffer))
    | Some buffer ->
        (match typ with
        | ArrayBuffer ->
            Stubs.Gl.bind_buffer Stubs.Gl.array_buffer buffer;
            ok { s with binded_vbo = Some buffer })

type purpose = StaticDraw

let purpose_to_gl = function StaticDraw -> Stubs.Gl.static_draw

let buffer_data typ data purpose s =
  let ba =
    Bigarray.Array1.of_array Bigarray.float32 Bigarray.c_layout
      (Array.of_list data)
  in
  match typ with
  | ArrayBuffer ->
      (match s.binded_vbo with
      | None -> error (Gl UnboundedBuffer)
      | Some _ ->
          ok
          @@ Stubs.Gl.buffer_data Stubs.Gl.array_buffer
               (Bigarray.Array1.size_in_bytes ba)
               (to_voidp @@ bigarray_start array1 ba)
               (purpose_to_gl purpose))

type data_type = Float

(* TODO: Some smart things to avoid errors *)
let vertex_attrib_pointer idx size typ normalized stride offset =
  check_error @@ fun () ->
  match typ with
  | Float ->
      Stubs.Gl.vertex_attrib_pointer idx size Stubs.Gl.float
        (if normalized then
           Stubs.Gl.true_
         else
           Stubs.Gl.false_)
        stride
        (ptr_of_raw_address @@ Nativeint.of_int offset)

let enable_vertex_attrib_array idx s =
  match s.binded_vao with
  | None -> error (Gl UnboundedVertexArray)
  | Some _ -> ok @@ Stubs.Gl.enable_vertex_attrib_array idx

let use_program s =
  match s.pid with
  | None -> error (Gl NoProgram)
  | Some pid -> ok @@ Stubs.Gl.use_program pid

let unbind_program _s = ok @@ Stubs.Gl.use_program 0

(* TODO: Make a GADT with all these values *)
type draw_kind = Triangles

let draw_arrays kind first count s =
  match s.binded_vao with
  | None -> error (Gl UnboundedVertexArray)
  | Some _ ->
      (match kind with
      | Triangles -> ok @@ Stubs.Gl.draw_arrays Stubs.Gl.triangles first count)

let finalise s =
  bind_buffer ArrayBuffer 0 s >>= bind_vertex_array 0
  >>= fun ({ vaos; _ } as gl) ->
  Array.of_list (List.map Int32.of_int vaos)
  |> Bigarray.Array1.of_array Bigarray.int32 Bigarray.c_layout
  |> fun a ->
  Stubs.Gl.delete_vertex_arrays (Bigarray.Array1.dim a) a;
  ok { gl with vaos = [] } >>= fun ({ buffers; _ } as gl) ->
  Array.of_list (List.map Int32.of_int buffers)
  |> Bigarray.Array1.of_array Bigarray.int32 Bigarray.c_layout
  |> fun a ->
  Stubs.Gl.delete_buffers (Bigarray.Array1.dim a) a;
  ok { gl with buffers = [] } >>= fun ({ pid; _ } as gl) ->
  (match pid with Some pid -> Stubs.Gl.delete_program pid | None -> ());
  ok { gl with pid = None }

let get_vaos s = s.vaos
let get_buffers s = s.buffers
let binded_vao s = s.binded_vao
let binded_vbo s = s.binded_vbo
