type glfw_error =
  | Not_initialized
      [@printer fun fmt () -> Format.fprintf fmt "GLFW not initialized@\n"]
  | No_context of string
      [@printer
        fun fmt s ->
          Format.fprintf fmt "GLFW did not set a current context@\n%s@\n" s]
  | Initialization_failed
      [@printer
        fun fmt () -> Format.fprintf fmt "GLFW Initialization failed@\n"]
[@@deriving show]

type glew_error =
  | No_gl_version
  | No_gl_version_10_only
  | No_glx_version_11_only
  | No_glx_display
  | Unknown of int
[@@deriving show]

type gl_error =
  | Invalid_enum
  | Invalid_value
  | Invalid_operation
  | Invalid_framebuffer_operation
  | Out_of_memory
  | Stack_underflow
  | Stack_overflow
  | ShaderCompilationFailed of string
  | ProgramLinkingFailed of string
  | UnknownVertexArray of int
  | UnknownBuffer of int
  | UnboundedBuffer
  | UnboundedVertexArray
  | NoProgram
  | NotImplementedYet
[@@deriving show]

type oengine_error = UnknownState | NoBuffer | BufferNotFound
[@@deriving show]

type error_kind =
  | Glfw of glfw_error
      [@printer fun fmt e -> Format.fprintf fmt "[GLFW]: %a@\n" pp_glfw_error e]
  | Gl of gl_error
      [@printer fun fmt e -> Format.fprintf fmt "[GL]: %a@\n" pp_gl_error e]
  | Glew of glew_error
      [@printer fun fmt e -> Format.fprintf fmt "[GLEW]: %a@\n" pp_glew_error e]
  | OEngine of oengine_error
      [@printer
        fun fmt e -> Format.fprintf fmt "[OEngine]: %a@\n" pp_oengine_error e]
[@@deriving show]

type 'a t = ('a, error_kind) result [@@deriving show]
