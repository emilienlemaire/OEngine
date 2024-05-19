open Ctypes

module Types = struct
  module Gl = struct
    type ('a, 'b) bigarray = ('a, 'b, Bigarray.c_layout) Bigarray.Array1.t
    type enum = int
    type bitfield = Unsigned.uint

    let bitfield : bitfield typ = typedef uint "GLbitfield"

    type uint = Unsigned.uint

    let uint : uint typ = typedef uint "GLuint"

    type sizei = int

    let sizei : sizei typ = typedef int "GLsizei"

    type nonrec int = int

    let int : int typ = typedef int "GLsizei"

    type clampf = float

    let clampf : clampf typ = typedef float "GLclampf"

    type nonrec char = char

    let char : char typ = typedef char "GLchar"

    let int_as_uint =
      view ~read:Unsigned.UInt.to_int ~write:Unsigned.UInt.of_int uint

    let ba_as_charp =
      view
        ~read:(fun _ : (char, Bigarray.int8_unsigned_elt) bigarray ->
          assert false)
        ~write:(fun b -> to_voidp (bigarray_start array1 b))
        (ptr void)

    let ba_as_int32p =
      view
        ~read:(fun _ : (int32, Bigarray.int32_elt) bigarray -> assert false)
        ~write:(fun b -> to_voidp (bigarray_start array1 b))
        (ptr void)

    let ba_opt_as_int32p =
      view
        ~read:(fun _ : (int32, Bigarray.int32_elt) bigarray option ->
          assert false)
        ~write:(function
          | None -> null | Some b -> to_voidp (bigarray_start array1 b))
        (ptr void)

    let ba_as_uint32p =
      view
        ~read:(fun _ : (int32, Bigarray.int32_elt) bigarray -> assert false)
        ~write:(fun b -> to_voidp (bigarray_start array1 b))
        (ptr void)
  end

  module Glfw = struct
    (* 1365 *)
    type glproc = unit -> unit

    let glproc : glproc typ =
      typedef (Foreign.funptr (void @-> returning void)) "GLFWglproc"

    (* 1391 *)
    type monitor

    let monitor : monitor structure typ =
      typedef (structure "GLFWmonitor") "GLFWmonitor"

    (* 1403 *)
    type window

    let window : window structure typ =
      typedef (structure "GLFWwindow") "GLFWwindow"

    (* 1590 *)
    type error_fun = int -> string -> unit

    let error_fun : error_fun typ =
      typedef
        (Foreign.funptr (int @-> string @-> returning void))
        "GLFWerrorfun"

    (* 1635 *)
    type window_size_fun = window structure ptr -> int -> int -> unit

    let window_size_fun : window_size_fun typ =
      typedef
        (Foreign.funptr (ptr window @-> int @-> int @-> returning void))
        "GLFWwindowsizefun"

    (* 1655 *)
    type window_close_fun = window structure ptr -> unit

    let window_close_fun : window_close_fun typ =
      typedef
        (Foreign.funptr (ptr window @-> returning void))
        "GLFWwindowclosefun"

    (* 1696 *)
    type window_focus_fun = window structure ptr -> int -> unit

    let window_focus_fun : window_focus_fun typ =
      typedef
        (Foreign.funptr (ptr window @-> int @-> returning void))
        "GLFWwindowfocusfun"

    (* 1759 *)
    type framebuffer_size_fun = window structure ptr -> int -> int -> unit

    let framebuffer_size_fun : framebuffer_size_fun typ =
      typedef
        (Foreign.funptr (ptr window @-> int @-> int @-> returning void))
        "GLFWframebuffersizefun"

    (* 1806 *)
    type mouse_button_fun = window structure ptr -> int -> int -> int -> unit

    let mouse_button_fun : mouse_button_fun typ =
      typedef
        (Foreign.funptr (ptr window @-> int @-> int @-> int @-> returning void))
        "GLFWmousebuttonfun"

    (* 1829 *)
    type cursor_pos_fun = window structure ptr -> float -> float -> unit

    let cursor_pos_fun : cursor_pos_fun typ =
      typedef
        (Foreign.funptr (ptr window @-> double @-> double @-> returning void))
        "GLFWcursorposfun"

    (* 1871 *)
    type scroll_fun = window structure ptr -> float -> float -> unit

    let scroll_fun : scroll_fun typ =
      typedef
        (Foreign.funptr (ptr window @-> double @-> double @-> returning void))
        "GLFWscrollfun"

    (* 1897 *)
    type key_fun = window structure ptr -> int -> int -> int -> int -> unit

    let key_fun : key_fun typ =
      typedef
        (Foreign.funptr
           (ptr window @-> int @-> int @-> int @-> int @-> returning void))
        "GLFWkeyfun"
  end
end

module Constants (T : Ctypes.TYPE) = struct
  module Gl = struct
    open Types
    open T

    (* 328 *)
    let false_ = constant "GL_FALSE" @@ T.lift_typ Gl.int

    (* 332 *)
    let no_error = constant "GL_NO_ERROR" int
    let points = constant "GL_POINTS" int

    (* 335 *)
    let true_ = constant "GL_TRUE" int

    (* 338 *)
    let lines = constant "GL_LINES" int

    (* 343 *)
    let line_strip = constant "GL_LINE_STRIP" int

    (* 344 *)
    let triangles = constant "GL_TRIANGLES" int
    let triangle_strip = constant "GL_TRIANGLE_STRIP" int

    (* 347 *)
    let quads = constant "GL_QUADS" int

    (* 355 *)
    let depth_buffer_bit = constant "GL_DEPTH_BUFFER_BIT" int

    (* 364 *)
    let equal = constant "GL_EQUAL" int

    (* 393 *)
    let invalid_enum = constant "GL_INVALID_ENUM" int
    let invalid_value = constant "GL_INVALID_VALUE" int
    let invalid_operation = constant "GL_INVALID_OPERATION" int
    let stack_overflow = constant "GL_STACK_OVERFLOW" int
    let stack_underflow = constant "GL_STACK_UNDERFLOW" int
    let out_of_memory = constant "GL_OUT_OF_MEMORY" int

    (* 415 *)
    let cw = constant "GL_CW" int
    let ccw = constant "GL_CCW" int

    (* 653 *)
    let unsigned_short = constant "GL_UNSIGNED_SHORT" int
    let int_ = constant "GL_INT" int
    let unsigned_int = constant "GL_UNSIGNED_INT" int
    let float = constant "GL_FLOAT" int

    (* 711*)
    let vendor = constant "GL_VENDOR" int
    let renderer = constant "GL_RENDERER" int
    let version = constant "GL_VERSION" int

    (* 768 *)
    let color_buffer_bit = constant "GL_COLOR_BUFFER_BIT" int

    (* 1662 *)
    let array_buffer = constant "GL_ARRAY_BUFFER" int
    let element_array_buffer = constant "GL_ELEMENT_ARRAY_BUFFER" int

    (* 1685 *)
    let static_draw = constant "GL_STATIC_DRAW" int

    (* 1688 *)
    let dynamic_draw = constant "GL_DYNAMIC_DRAW" int

    (* 1782 *)
    let fragment_shader = constant "GL_FRAGMENT_SHADER" int
    let vertex_shader = constant "GL_VERTEX_SHADER" int

    (* 1789 *)
    let shader_type = constant "GL_SHADER_TYPE" int

    (* 1796 *)
    let bool_ = constant "GL_BOOL" int

    (* 1809 *)
    let delete_status = constant "GL_DELETE_STATUS" int
    let compile_status = constant "GL_COMPILE_STATUS" int
    let link_status = constant "GL_LINK_STATUS" int
    let validate_status = constant "GL_VALIDATE_STATUS" int
    let info_log_length = constant "GL_INFO_LOG_LENGTH" int
    let attached_shaders = constant "GL_ATTACHED_SHADERS" int
    let active_uniforms = constant "GL_ACTIVE_UNIFORMS" int
    let active_uniform_max_length = constant "GL_ACTIVE_UNIFORM_MAX_LENGTH" int
    let shader_source_length = constant "GL_SHADER_SOURCE_LENGTH" int
    let active_attributes = constant "GL_ACTIVE_ATTRIBUTES" int

    let active_attribute_max_length =
      constant "GL_ACTIVE_ATTRIBUTE_MAX_LENGTH" int

    (* 1821*)
    let shading_language_version = constant "GL_SHADING_LANGUAGE_VERSION" int

    (* 2120 *)
    let transform_feedback_varying_max_length =
      constant "GL_TRANSFORM_FEEDBACK_VARYING_MAX_LENGTH" int

    let transform_feedback_buffer_mode =
      constant "GL_TRANSFORM_FEEDBACK_BUFFER_MODE" int

    (* 2123 *)
    let transform_feedback_varyings =
      constant "GL_TRANSFORM_FEEDBACK_VARYINGS" int

    (* 2131 *)
    let interleaved_attribs = constant "GL_INTERLEAVED_ATTRIBS" int
    let separate_attribs = constant "GL_SEPARATE_ATTRIBS" int

    (* 2357 *)
    let lines_adjacency = constant "GL_LINES_ADJACENCY" int

    (* 2359 *)
    let triangles_adjacency = constant "GL_TRIANGLES_ADJACENCY" int

    (* 2362 *)
    let geometry_vertices_out = constant "GL_GEOMETRY_VERTICES_OUT" int
    let geometry_input_type = constant "GL_GEOMETRY_INPUT_TYPE" int
    let geometry_output_type = constant "GL_GEOMETRY_OUTPUT_TYPE" int

    (* 2368 *)
    let geometry_shader = constant "GL_GEOMETRY_SHADER" int

    (* 2537 *)
    let spir_v_binary = constant "GL_SPIR_V_BINARY" int

    (* 4221 *)
    let compute_work_group_size = constant "GL_COMPUTE_WORK_GROUP_SIZE" int

    (* 4227 *)
    let compute_shader = constant "GL_COMPUTE_SHADER" int

    (* 4863 *)
    let invalid_framebuffer_operation =
      constant "GL_INVALID_FRAMEBUFFER_OPERATION" int

    (* 5039 *)
    let program_binary_retrievable_hint =
      constant "GL_PROGRAM_BINARY_RETRIEVABLE_HINT" int

    let program_binary_length = constant "GL_PROGRAM_BINARY_LENGTH" int

    (* 5092 *)
    let geometry_shader_invocations =
      constant "GL_GEOMETRY_SHADER_INVOCATIONS" int

    (* 6312 *)
    let program_separable = constant "GL_PROGRAM_SEPARABLE" int

    (* 6482 *)
    let active_atomic_counter_buffers =
      constant "GL_ACTIVE_ATOMIC_COUNTER_BUFFERS" int

    (* 7086 *)
    let tess_control_output_vertices =
      constant "GL_TESS_CONTROL_OUTPUT_VERTICES" int

    let tess_gen_mode = constant "GL_TESS_GEN_MODE" int
    let tess_gen_spacing = constant "GL_TESS_GEN_SPACING" int
    let tess_gen_vertex_order = constant "GL_TESS_GEN_VERTEX_ORDER" int
    let tess_gen_point_mode = constant "GL_TESS_GEN_POINT_MODE" int
    let isolines = constant "GL_ISOLINES" int
    let fractional_odd = constant "GL_FRACTIONAL_ODD" int
    let fractional_even = constant "GL_FRACTIONAL_EVEN" int

    (* 7105 *)
    let tess_evaluation_shader = constant "GL_TESS_EVALUATION_SHADER" int

    (* 7105 *)
    let tess_control_shader = constant "GL_TESS_CONTROL_SHADER" int

    (* 7800 *)
    let active_uniform_block_max_name_length =
      constant "GL_ACTIVE_UNIFORM_BLOCK_MAX_NAME_LENGTH" int

    let active_uniform_blocks = constant "GL_ACTIVE_UNIFORM_BLOCKS" int
  end

  module Glfw = struct
    open T

    (* 312 *)
    let true_ = constant "GLFW_TRUE" int

    (* 321 *)
    let false_ = constant "GLFW_FALSE" int

    (* 331 *)
    let release = constant "GLFW_RELEASE" int

    (* 338 *)
    let press = constant "GLFW_PRESS" int

    (* 345 *)
    let repeat = constant "GLFW_REPEAT" int

    (* 448 *)
    let key_space = constant "GLFW_KEY_SPACE" int
    let key_apostrophe = constant "GLFW_KEY_APOSTROPHE" int
    let key_comma = constant "GLFW_KEY_COMMA" int
    let key_minus = constant "GLFW_KEY_MINUS" int
    let key_period = constant "GLFW_KEY_PERIOD" int
    let key_slash = constant "GLFW_KEY_SLASH" int
    let key_0 = constant "GLFW_KEY_0" int
    let key_1 = constant "GLFW_KEY_1" int
    let key_2 = constant "GLFW_KEY_2" int
    let key_3 = constant "GLFW_KEY_3" int
    let key_4 = constant "GLFW_KEY_4" int
    let key_5 = constant "GLFW_KEY_5" int
    let key_6 = constant "GLFW_KEY_6" int
    let key_7 = constant "GLFW_KEY_7" int
    let key_8 = constant "GLFW_KEY_8" int
    let key_9 = constant "GLFW_KEY_9" int
    let key_semicolon = constant "GLFW_KEY_SEMICOLON" int
    let key_equal = constant "GLFW_KEY_EQUAL" int
    let key_a = constant "GLFW_KEY_A" int
    let key_b = constant "GLFW_KEY_B" int
    let key_c = constant "GLFW_KEY_C" int
    let key_d = constant "GLFW_KEY_D" int
    let key_e = constant "GLFW_KEY_E" int
    let key_f = constant "GLFW_KEY_F" int
    let key_g = constant "GLFW_KEY_G" int
    let key_h = constant "GLFW_KEY_H" int
    let key_i = constant "GLFW_KEY_I" int
    let key_j = constant "GLFW_KEY_J" int
    let key_k = constant "GLFW_KEY_K" int
    let key_l = constant "GLFW_KEY_L" int
    let key_m = constant "GLFW_KEY_M" int
    let key_n = constant "GLFW_KEY_N" int
    let key_o = constant "GLFW_KEY_O" int
    let key_p = constant "GLFW_KEY_P" int
    let key_q = constant "GLFW_KEY_Q" int
    let key_r = constant "GLFW_KEY_R" int
    let key_s = constant "GLFW_KEY_S" int
    let key_t = constant "GLFW_KEY_T" int
    let key_u = constant "GLFW_KEY_U" int
    let key_v = constant "GLFW_KEY_V" int
    let key_w = constant "GLFW_KEY_W" int
    let key_x = constant "GLFW_KEY_X" int
    let key_y = constant "GLFW_KEY_Y" int
    let key_z = constant "GLFW_KEY_Z" int
    let key_left_bracket = constant "GLFW_KEY_LEFT_BRACKET" int
    let key_backslash = constant "GLFW_KEY_BACKSLASH" int
    let key_right_bracket = constant "GLFW_KEY_RIGHT_BRACKET" int
    let key_grave_accent = constant "GLFW_KEY_GRAVE_ACCENT" int
    let key_world_1 = constant "GLFW_KEY_WORLD_1" int
    let key_world_2 = constant "GLFW_KEY_WORLD_2" int
    let key_escape = constant "GLFW_KEY_ESCAPE" int
    let key_enter = constant "GLFW_KEY_ENTER" int
    let key_tab = constant "GLFW_KEY_TAB" int
    let key_backspace = constant "GLFW_KEY_BACKSPACE" int
    let key_insert = constant "GLFW_KEY_INSERT" int
    let key_delete = constant "GLFW_KEY_DELETE" int
    let key_right = constant "GLFW_KEY_RIGHT" int
    let key_left = constant "GLFW_KEY_LEFT" int
    let key_down = constant "GLFW_KEY_DOWN" int
    let key_up = constant "GLFW_KEY_UP" int
    let key_page_up = constant "GLFW_KEY_PAGE_UP" int
    let key_page_down = constant "GLFW_KEY_PAGE_DOWN" int
    let key_home = constant "GLFW_KEY_HOME" int
    let key_end = constant "GLFW_KEY_END" int
    let key_caps_lock = constant "GLFW_KEY_CAPS_LOCK" int
    let key_scroll_lock = constant "GLFW_KEY_SCROLL_LOCK" int
    let key_num_lock = constant "GLFW_KEY_NUM_LOCK" int
    let key_print_screen = constant "GLFW_KEY_PRINT_SCREEN" int
    let key_pause = constant "GLFW_KEY_PAUSE" int
    let key_f1 = constant "GLFW_KEY_F1" int
    let key_f2 = constant "GLFW_KEY_F2" int
    let key_f3 = constant "GLFW_KEY_F3" int
    let key_f4 = constant "GLFW_KEY_F4" int
    let key_f5 = constant "GLFW_KEY_F5" int
    let key_f6 = constant "GLFW_KEY_F6" int
    let key_f7 = constant "GLFW_KEY_F7" int
    let key_f8 = constant "GLFW_KEY_F8" int
    let key_f9 = constant "GLFW_KEY_F9" int
    let key_f10 = constant "GLFW_KEY_F10" int
    let key_f11 = constant "GLFW_KEY_F11" int
    let key_f12 = constant "GLFW_KEY_F12" int
    let key_f13 = constant "GLFW_KEY_F13" int
    let key_f14 = constant "GLFW_KEY_F14" int
    let key_f15 = constant "GLFW_KEY_F15" int
    let key_f16 = constant "GLFW_KEY_F16" int
    let key_f17 = constant "GLFW_KEY_F17" int
    let key_f18 = constant "GLFW_KEY_F18" int
    let key_f19 = constant "GLFW_KEY_F19" int
    let key_f20 = constant "GLFW_KEY_F20" int
    let key_f21 = constant "GLFW_KEY_F21" int
    let key_f22 = constant "GLFW_KEY_F22" int
    let key_f23 = constant "GLFW_KEY_F23" int
    let key_f24 = constant "GLFW_KEY_F24" int
    let key_f25 = constant "GLFW_KEY_F25" int
    let key_kp_0 = constant "GLFW_KEY_KP_0" int
    let key_kp_1 = constant "GLFW_KEY_KP_1" int
    let key_kp_2 = constant "GLFW_KEY_KP_2" int
    let key_kp_3 = constant "GLFW_KEY_KP_3" int
    let key_kp_4 = constant "GLFW_KEY_KP_4" int
    let key_kp_5 = constant "GLFW_KEY_KP_5" int
    let key_kp_6 = constant "GLFW_KEY_KP_6" int
    let key_kp_7 = constant "GLFW_KEY_KP_7" int
    let key_kp_8 = constant "GLFW_KEY_KP_8" int
    let key_kp_9 = constant "GLFW_KEY_KP_9" int
    let key_kp_decimal = constant "GLFW_KEY_KP_DECIMAL" int
    let key_kp_divide = constant "GLFW_KEY_KP_DIVIDE" int
    let key_kp_multiply = constant "GLFW_KEY_KP_MULTIPLY" int
    let key_kp_subtract = constant "GLFW_KEY_KP_SUBTRACT" int
    let key_kp_add = constant "GLFW_KEY_KP_ADD" int
    let key_kp_enter = constant "GLFW_KEY_KP_ENTER" int
    let key_kp_equal = constant "GLFW_KEY_KP_EQUAL" int
    let key_left_shift = constant "GLFW_KEY_LEFT_SHIFT" int
    let key_left_control = constant "GLFW_KEY_LEFT_CONTROL" int
    let key_left_alt = constant "GLFW_KEY_LEFT_ALT" int
    let key_left_super = constant "GLFW_KEY_LEFT_SUPER" int
    let key_right_shift = constant "GLFW_KEY_RIGHT_SHIFT" int
    let key_right_control = constant "GLFW_KEY_RIGHT_CONTROL" int
    let key_right_alt = constant "GLFW_KEY_RIGHT_ALT" int
    let key_right_super = constant "GLFW_KEY_RIGHT_SUPER" int
    let key_menu = constant "GLFW_KEY_MENU" int

    (* 535 *)
    let mod_shift = constant "GLFW_MOD_SHIFT" int

    (* 540 *)
    let mod_control = constant "GLFW_MOD_CONTROL" int

    (* 545 *)
    let mod_alt = constant "GLFW_MOD_ALT" int

    (* 550 *)
    let mod_super = constant "GLFW_MOD_SUPER" int

    (* 556 *)
    let mod_caps_lock = constant "GLFW_MOD_CAPS_LOCK" int

    (* 562 *)
    let mod_num_lock = constant "GLFW_MOD_NUM_LOCK" int

    (* 573 *)
    let mouse_button_1 = constant "GLFW_MOUSE_BUTTON_1" int
    let mouse_button_2 = constant "GLFW_MOUSE_BUTTON_2" int
    let mouse_button_3 = constant "GLFW_MOUSE_BUTTON_3" int
    let mouse_button_4 = constant "GLFW_MOUSE_BUTTON_4" int
    let mouse_button_5 = constant "GLFW_MOUSE_BUTTON_5" int
    let mouse_button_6 = constant "GLFW_MOUSE_BUTTON_6" int
    let mouse_button_7 = constant "GLFW_MOUSE_BUTTON_7" int
    let mouse_button_8 = constant "GLFW_MOUSE_BUTTON_8" int

    (* 1037 *)
    let context_version_major = constant "GLFW_CONTEXT_VERSION_MAJOR" int

    (* 1043 *)
    let context_version_minor = constant "GLFW_CONTEXT_VERSION_MINOR" int

    (* 1078 *)
    let opengl_profile = constant "GLFW_OPENGL_PROFILE" int

    (* 1148 *)
    let opengl_any_profile = constant "GLFW_OPENGL_ANY_PROFILE" int
    let opengl_core_profile = constant "GLFW_OPENGL_CORE_PROFILE" int
    let opengl_compat_profile = constant "GLFW_OPENGL_COMPAT_PROFILE" int
  end

  module Glew = struct
    open T

    let ok = constant "GLEW_OK" int
    let no_error = constant "GLEW_NO_ERROR" int
    let error_no_gl_version = constant "GLEW_ERROR_NO_GL_VERSION" int
    let error_gl_version_10_only = constant "GLEW_ERROR_GL_VERSION_10_ONLY" int

    let error_glx_version_11_only =
      constant "GLEW_ERROR_GLX_VERSION_11_ONLY" int

    let error_no_glx_display = constant "GLEW_ERROR_NO_GLX_DISPLAY" int
  end
end

module Bindings (F : Ctypes.FOREIGN) = struct
  module Glfw = struct
    open Types.Glfw
    open F

    (* 2220 *)
    let init = foreign "glfwInit" (void @-> returning int)

    (* 2254 *)
    let terminate = foreign "glfwTerminate" (void @-> returning void)

    (* 2503 *)
    let set_error_callback =
      foreign "glfwSetErrorCallback" (error_fun @-> returning error_fun)

    (* 3054 *)
    let window_hint = foreign "glfwWindowHint" (int @-> int @-> returning void)

    (* 3235 *)
    let create_window =
      foreign "glfwCreateWindow"
        (int @-> int @-> string @-> ptr_opt monitor @-> ptr_opt window
        @-> returning (ptr window))

    (* 3264 *)
    let destroy_window =
      foreign "glfwDestroyWindow" (ptr window @-> returning void)

    (* 3284 *)
    let window_should_close =
      foreign "glfwWindowShouldClose" (ptr window @-> returning int)

    (* 3306 *)
    let set_window_should_close =
      foreign "glfwSetWindowShouldClose" (ptr window @-> int @-> returning void)

    (* 4176 *)
    let set_window_user_pointer =
      foreign "glfwSetWindowUserPointer"
        (ptr window @-> ptr void @-> returning void)

    (* 4197 *)
    let get_window_user_pointer =
      foreign "glfwGetWindowUserPointer" (ptr window @-> returning (ptr void))

    (* 4264 *)
    let set_window_size_callback =
      foreign "glfwSetWindowSizeCallback"
        (ptr window @-> window_size_fun @-> returning window_size_fun)

    (* 4304 *)
    let set_window_close_callback =
      foreign "glfwSetWindowCloseCallback"
        (ptr window @-> window_close_fun @-> returning window_close_fun)

    (* 4375 *)
    let set_window_focus_callback =
      foreign "glfwSetWindowFocusCallback"
        (ptr window @-> window_focus_fun @-> returning window_focus_fun)

    (* 4465 *)
    let set_framebuffer_size_callback =
      foreign "glfwSetFramebufferSizeCallback"
        (ptr window @-> framebuffer_size_fun @-> returning framebuffer_size_fun)

    (* 4534 *)
    let poll_events = foreign "glfwPollEvents" (void @-> returning void)

    (* 5198 *)
    let set_key_callback =
      foreign "glfwSetKeyCallback" (ptr window @-> key_fun @-> returning key_fun)

    (* 5320 *)
    let set_mouse_button_callback =
      foreign "glfwSetMouseButtonCallback"
        (ptr window @-> mouse_button_fun @-> returning mouse_button_fun)

    (* 5352 *)
    let set_cursor_pos_callback =
      foreign "glfwSetCursorPosCallback"
        (ptr window @-> cursor_pos_fun @-> returning cursor_pos_fun)

    (* 5417 *)
    let set_scroll_callback =
      foreign "glfwSetScrollCallback"
        (ptr window @-> scroll_fun @-> returning scroll_fun)

    (* 6109 *)
    let make_context_current =
      foreign "glfwMakeContextCurrent" (ptr window @-> returning void)

    (* 6164 *)
    let swap_buffers = foreign "glfwSwapBuffers" (ptr window @-> returning void)

    (* 6210 *)
    let swap_interval = foreign "glfwSwapInterval" (int @-> returning void)

    (* 6290 *)
    let get_proc_address =
      foreign "glfwGetProcAddress" (string @-> returning glproc)
  end

  module Glew = struct
    open Types.Gl
    open F

    let init = foreign "glewInit" (void @-> returning int_as_uint)
  end

  module Gl = struct
    open Types.Gl
    open F

    (* 872 *)
    let clear = foreign "glClear" (int_as_uint @-> returning void)

    (* 874 *)
    let clear_color =
      foreign "glClearColor"
        (clampf @-> clampf @-> clampf @-> clampf @-> returning void)

    (* 927 *)
    let draw_arrays =
      foreign "glDrawArrays" (int_as_uint @-> int @-> int @-> returning void)

    (* 929 *)
    let draw_elements =
      foreign "glDrawElements"
        (int_as_uint @-> int @-> int_as_uint @-> ptr_opt void @-> returning void)

    (* 964 *)
    let get_error = foreign "glGetError" (void @-> returning int_as_uint)

    (* 979 *)
    let get_string = foreign "glGetString" (int_as_uint @-> returning string_opt)

    (* 1197 *)
    let viewport =
      foreign "glViewport" (int @-> int @-> int @-> int @-> returning void)

    (* 1717 *)
    let bind_buffer =
      foreign "glBindBuffer" (int_as_uint @-> int_as_uint @-> returning void)

    let buffer_data =
      foreign "glBufferData"
        (int_as_uint @-> int @-> ptr void @-> int_as_uint @-> returning void)

    let buffer_sub_data =
      foreign "glBufferSubData"
        (int_as_uint @-> int @-> int @-> ptr void @-> returning void)

    let delete_buffers =
      foreign "glDeleteBuffers" (int @-> ba_as_uint32p @-> returning void)

    (* 1723 *)
    let gen_buffers =
      foreign "glGenBuffers" (int @-> ba_as_int32p @-> returning void)

    (* 1924 *)
    let attach_shader =
      foreign "glAttachShader" (int_as_uint @-> int_as_uint @-> returning void)

    (* 1927 *)
    let compile_shader =
      foreign "glCompileShader" (int_as_uint @-> returning void)

    let create_program =
      foreign "glCreateProgram" (void @-> returning int_as_uint)

    let create_shader =
      foreign "glCreateShader" (int_as_uint @-> returning int_as_uint)

    let delete_program =
      foreign "glDeleteProgram" (int_as_uint @-> returning void)

    let delete_shader = foreign "glDeleteShader" (int_as_uint @-> returning void)

    let detach_shader =
      foreign "glDetachShader" (int_as_uint @-> int_as_uint @-> returning void)

    (* 1935 *)
    let enable_vertex_attrib_array =
      foreign "glEnableVertexAttribArray" (int_as_uint @-> returning void)

    (* 1940 *)
    let get_program_info_log =
      foreign "glGetProgramInfoLog"
        (int_as_uint @-> int @-> ba_opt_as_int32p @-> ba_as_charp
       @-> returning void)

    let get_programiv =
      foreign "glGetProgramiv"
        (int_as_uint @-> int_as_uint @-> ba_as_int32p @-> returning void)

    let get_shader_info_log =
      foreign "glGetShaderInfoLog"
        (int_as_uint @-> int @-> ba_opt_as_int32p @-> ba_as_charp
       @-> returning void)

    let get_shader_source =
      foreign "glGetShaderSource"
        (int_as_uint @-> int @-> ba_opt_as_int32p @-> ba_as_charp
       @-> returning void)

    let get_shaderiv =
      foreign "glGetShaderiv"
        (int_as_uint @-> int_as_uint @-> ba_as_int32p @-> returning void)

    (* 1954 *)
    let link_program = foreign "glLinkProgram" (int_as_uint @-> returning void)

    let shader_source =
      foreign "glShaderSource"
        (int_as_uint @-> int @-> ptr string @-> ptr_opt void @-> returning void)

    (* 1978 *)
    let use_program = foreign "glUseProgram" (int_as_uint @-> returning void)

    (* 4548 *)
    let create_vertex_arrays =
      foreign "glCreateVertexArrays" (int @-> ba_as_uint32p @-> returning void)

    (* 2016 *)
    let vertex_attrib_pointer =
      foreign "glVertexAttribPointer"
        (int_as_uint @-> int @-> int_as_uint @-> int @-> int @-> ptr void
       @-> returning void)

    (* 7871 *)
    let bind_vertex_array =
      foreign "glBindVertexArray" (int_as_uint @-> returning void)

    let delete_vertex_arrays =
      foreign "glDeleteVertexArrays" (int @-> ba_as_uint32p @-> returning void)

    let gen_vertex_arrays =
      foreign "glGenVertexArrays" (int @-> ba_as_int32p @-> returning void)
  end
end
