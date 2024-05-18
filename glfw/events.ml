(* open React *)

type 'a event = Live of 'a | Handled of 'a [@@deriving show]

module Actions = struct
  type t = RELEASE | PRESS | REPEAT [@@deriving show]

  let of_glfw = function
    | x when x = Stubs.Glfw.release -> RELEASE
    | x when x = Stubs.Glfw.press -> PRESS
    | x when x = Stubs.Glfw.repeat -> REPEAT
    | _ -> invalid_arg __FUNCTION__

  let to_glfw = function
    | RELEASE -> Stubs.Glfw.release
    | PRESS -> Stubs.Glfw.press
    | REPEAT -> Stubs.Glfw.repeat
end

module Keys = struct
  type t =
    | SPACE
    | APOSTROPHE
    | COMMA
    | MINUS
    | PERIOD
    | SLASH
    | NUM_0
    | NUM_1
    | NUM_2
    | NUM_3
    | NUM_4
    | NUM_5
    | NUM_6
    | NUM_7
    | NUM_8
    | NUM_9
    | SEMICOLON
    | EQUAL
    | A
    | B
    | C
    | D
    | E
    | F
    | G
    | H
    | I
    | J
    | K
    | L
    | M
    | N
    | O
    | P
    | Q
    | R
    | S
    | T
    | U
    | V
    | W
    | X
    | Y
    | Z
    | LEFT_BRACKET
    | BACKSLASH
    | RIGHT_BRACKET
    | GRAVE_ACCENT
    | WORLD_1
    | WORLD_2
    | ESCAPE
    | ENTER
    | TAB
    | BACKSPACE
    | INSERT
    | DELETE
    | RIGHT
    | LEFT
    | DOWN
    | UP
    | PAGE_UP
    | PAGE_DOWN
    | HOME
    | END
    | CAPS_LOCK
    | SCROLL_LOCK
    | NUM_LOCK
    | PRINT_SCREEN
    | PAUSE
    | F1
    | F2
    | F3
    | F4
    | F5
    | F6
    | F7
    | F8
    | F9
    | F10
    | F11
    | F12
    | F13
    | F14
    | F15
    | F16
    | F17
    | F18
    | F19
    | F20
    | F21
    | F22
    | F23
    | F24
    | F25
    | KP_0
    | KP_1
    | KP_2
    | KP_3
    | KP_4
    | KP_5
    | KP_6
    | KP_7
    | KP_8
    | KP_9
    | KP_DECIMAL
    | KP_DIVIDE
    | KP_MULTIPLY
    | KP_SUBTRACT
    | KP_ADD
    | KP_ENTER
    | KP_EQUAL
    | LEFT_SHIFT
    | LEFT_CONTROL
    | LEFT_ALT
    | LEFT_SUPER
    | RIGHT_SHIFT
    | RIGHT_CONTROL
    | RIGHT_ALT
    | RIGHT_SUPER
    | MENU
  [@@deriving show]

  let of_glfw = function
    | x when x = Stubs.Glfw.key_space -> SPACE
    | x when x = Stubs.Glfw.key_apostrophe -> APOSTROPHE
    | x when x = Stubs.Glfw.key_comma -> COMMA
    | x when x = Stubs.Glfw.key_minus -> MINUS
    | x when x = Stubs.Glfw.key_period -> PERIOD
    | x when x = Stubs.Glfw.key_slash -> SLASH
    | x when x = Stubs.Glfw.key_0 -> NUM_0
    | x when x = Stubs.Glfw.key_1 -> NUM_1
    | x when x = Stubs.Glfw.key_2 -> NUM_2
    | x when x = Stubs.Glfw.key_3 -> NUM_3
    | x when x = Stubs.Glfw.key_4 -> NUM_4
    | x when x = Stubs.Glfw.key_5 -> NUM_5
    | x when x = Stubs.Glfw.key_6 -> NUM_6
    | x when x = Stubs.Glfw.key_7 -> NUM_7
    | x when x = Stubs.Glfw.key_8 -> NUM_8
    | x when x = Stubs.Glfw.key_9 -> NUM_9
    | x when x = Stubs.Glfw.key_semicolon -> SEMICOLON
    | x when x = Stubs.Glfw.key_equal -> EQUAL
    | x when x = Stubs.Glfw.key_a -> A
    | x when x = Stubs.Glfw.key_b -> B
    | x when x = Stubs.Glfw.key_c -> C
    | x when x = Stubs.Glfw.key_d -> D
    | x when x = Stubs.Glfw.key_e -> E
    | x when x = Stubs.Glfw.key_f -> F
    | x when x = Stubs.Glfw.key_g -> G
    | x when x = Stubs.Glfw.key_h -> H
    | x when x = Stubs.Glfw.key_i -> I
    | x when x = Stubs.Glfw.key_j -> J
    | x when x = Stubs.Glfw.key_k -> K
    | x when x = Stubs.Glfw.key_l -> L
    | x when x = Stubs.Glfw.key_m -> M
    | x when x = Stubs.Glfw.key_n -> N
    | x when x = Stubs.Glfw.key_o -> O
    | x when x = Stubs.Glfw.key_p -> P
    | x when x = Stubs.Glfw.key_q -> Q
    | x when x = Stubs.Glfw.key_r -> R
    | x when x = Stubs.Glfw.key_s -> S
    | x when x = Stubs.Glfw.key_t -> T
    | x when x = Stubs.Glfw.key_u -> U
    | x when x = Stubs.Glfw.key_v -> V
    | x when x = Stubs.Glfw.key_w -> W
    | x when x = Stubs.Glfw.key_x -> X
    | x when x = Stubs.Glfw.key_y -> Y
    | x when x = Stubs.Glfw.key_z -> Z
    | x when x = Stubs.Glfw.key_left_bracket -> LEFT_BRACKET
    | x when x = Stubs.Glfw.key_backslash -> BACKSLASH
    | x when x = Stubs.Glfw.key_right_bracket -> RIGHT_BRACKET
    | x when x = Stubs.Glfw.key_grave_accent -> GRAVE_ACCENT
    | x when x = Stubs.Glfw.key_world_1 -> WORLD_1
    | x when x = Stubs.Glfw.key_world_2 -> WORLD_2
    | x when x = Stubs.Glfw.key_escape -> ESCAPE
    | x when x = Stubs.Glfw.key_enter -> ENTER
    | x when x = Stubs.Glfw.key_tab -> TAB
    | x when x = Stubs.Glfw.key_backspace -> BACKSPACE
    | x when x = Stubs.Glfw.key_insert -> INSERT
    | x when x = Stubs.Glfw.key_delete -> DELETE
    | x when x = Stubs.Glfw.key_right -> RIGHT
    | x when x = Stubs.Glfw.key_left -> LEFT
    | x when x = Stubs.Glfw.key_down -> DOWN
    | x when x = Stubs.Glfw.key_up -> UP
    | x when x = Stubs.Glfw.key_page_up -> PAGE_UP
    | x when x = Stubs.Glfw.key_page_down -> PAGE_DOWN
    | x when x = Stubs.Glfw.key_home -> HOME
    | x when x = Stubs.Glfw.key_end -> END
    | x when x = Stubs.Glfw.key_caps_lock -> CAPS_LOCK
    | x when x = Stubs.Glfw.key_scroll_lock -> SCROLL_LOCK
    | x when x = Stubs.Glfw.key_num_lock -> NUM_LOCK
    | x when x = Stubs.Glfw.key_print_screen -> PRINT_SCREEN
    | x when x = Stubs.Glfw.key_pause -> PAUSE
    | x when x = Stubs.Glfw.key_f1 -> F1
    | x when x = Stubs.Glfw.key_f2 -> F2
    | x when x = Stubs.Glfw.key_f3 -> F3
    | x when x = Stubs.Glfw.key_f4 -> F4
    | x when x = Stubs.Glfw.key_f5 -> F5
    | x when x = Stubs.Glfw.key_f6 -> F6
    | x when x = Stubs.Glfw.key_f7 -> F7
    | x when x = Stubs.Glfw.key_f8 -> F8
    | x when x = Stubs.Glfw.key_f9 -> F9
    | x when x = Stubs.Glfw.key_f10 -> F10
    | x when x = Stubs.Glfw.key_f11 -> F11
    | x when x = Stubs.Glfw.key_f12 -> F12
    | x when x = Stubs.Glfw.key_f13 -> F13
    | x when x = Stubs.Glfw.key_f14 -> F14
    | x when x = Stubs.Glfw.key_f15 -> F15
    | x when x = Stubs.Glfw.key_f16 -> F16
    | x when x = Stubs.Glfw.key_f17 -> F17
    | x when x = Stubs.Glfw.key_f18 -> F18
    | x when x = Stubs.Glfw.key_f19 -> F19
    | x when x = Stubs.Glfw.key_f20 -> F20
    | x when x = Stubs.Glfw.key_f21 -> F21
    | x when x = Stubs.Glfw.key_f22 -> F22
    | x when x = Stubs.Glfw.key_f23 -> F23
    | x when x = Stubs.Glfw.key_f24 -> F24
    | x when x = Stubs.Glfw.key_f25 -> F25
    | x when x = Stubs.Glfw.key_kp_0 -> KP_0
    | x when x = Stubs.Glfw.key_kp_1 -> KP_1
    | x when x = Stubs.Glfw.key_kp_2 -> KP_2
    | x when x = Stubs.Glfw.key_kp_3 -> KP_3
    | x when x = Stubs.Glfw.key_kp_4 -> KP_4
    | x when x = Stubs.Glfw.key_kp_5 -> KP_5
    | x when x = Stubs.Glfw.key_kp_6 -> KP_6
    | x when x = Stubs.Glfw.key_kp_7 -> KP_7
    | x when x = Stubs.Glfw.key_kp_8 -> KP_8
    | x when x = Stubs.Glfw.key_kp_9 -> KP_9
    | x when x = Stubs.Glfw.key_kp_decimal -> KP_DECIMAL
    | x when x = Stubs.Glfw.key_kp_divide -> KP_DIVIDE
    | x when x = Stubs.Glfw.key_kp_multiply -> KP_MULTIPLY
    | x when x = Stubs.Glfw.key_kp_subtract -> KP_SUBTRACT
    | x when x = Stubs.Glfw.key_kp_add -> KP_ADD
    | x when x = Stubs.Glfw.key_kp_enter -> KP_ENTER
    | x when x = Stubs.Glfw.key_kp_equal -> KP_EQUAL
    | x when x = Stubs.Glfw.key_left_shift -> LEFT_SHIFT
    | x when x = Stubs.Glfw.key_left_control -> LEFT_CONTROL
    | x when x = Stubs.Glfw.key_left_alt -> LEFT_ALT
    | x when x = Stubs.Glfw.key_left_super -> LEFT_SUPER
    | x when x = Stubs.Glfw.key_right_shift -> RIGHT_SHIFT
    | x when x = Stubs.Glfw.key_right_control -> RIGHT_CONTROL
    | x when x = Stubs.Glfw.key_right_alt -> RIGHT_ALT
    | x when x = Stubs.Glfw.key_right_super -> RIGHT_SUPER
    | x when x = Stubs.Glfw.key_menu -> MENU
    | _ -> invalid_arg __FUNCTION__

  let to_glfw = function
    | SPACE -> Stubs.Glfw.key_space
    | APOSTROPHE -> Stubs.Glfw.key_apostrophe
    | COMMA -> Stubs.Glfw.key_comma
    | MINUS -> Stubs.Glfw.key_minus
    | PERIOD -> Stubs.Glfw.key_period
    | SLASH -> Stubs.Glfw.key_slash
    | NUM_0 -> Stubs.Glfw.key_0
    | NUM_1 -> Stubs.Glfw.key_1
    | NUM_2 -> Stubs.Glfw.key_2
    | NUM_3 -> Stubs.Glfw.key_3
    | NUM_4 -> Stubs.Glfw.key_4
    | NUM_5 -> Stubs.Glfw.key_5
    | NUM_6 -> Stubs.Glfw.key_6
    | NUM_7 -> Stubs.Glfw.key_7
    | NUM_8 -> Stubs.Glfw.key_8
    | NUM_9 -> Stubs.Glfw.key_9
    | SEMICOLON -> Stubs.Glfw.key_semicolon
    | EQUAL -> Stubs.Glfw.key_equal
    | A -> Stubs.Glfw.key_a
    | B -> Stubs.Glfw.key_b
    | C -> Stubs.Glfw.key_c
    | D -> Stubs.Glfw.key_d
    | E -> Stubs.Glfw.key_e
    | F -> Stubs.Glfw.key_f
    | G -> Stubs.Glfw.key_g
    | H -> Stubs.Glfw.key_h
    | I -> Stubs.Glfw.key_i
    | J -> Stubs.Glfw.key_j
    | K -> Stubs.Glfw.key_k
    | L -> Stubs.Glfw.key_l
    | M -> Stubs.Glfw.key_m
    | N -> Stubs.Glfw.key_n
    | O -> Stubs.Glfw.key_o
    | P -> Stubs.Glfw.key_p
    | Q -> Stubs.Glfw.key_q
    | R -> Stubs.Glfw.key_r
    | S -> Stubs.Glfw.key_s
    | T -> Stubs.Glfw.key_t
    | U -> Stubs.Glfw.key_u
    | V -> Stubs.Glfw.key_v
    | W -> Stubs.Glfw.key_w
    | X -> Stubs.Glfw.key_x
    | Y -> Stubs.Glfw.key_y
    | Z -> Stubs.Glfw.key_z
    | LEFT_BRACKET -> Stubs.Glfw.key_left_bracket
    | BACKSLASH -> Stubs.Glfw.key_backslash
    | RIGHT_BRACKET -> Stubs.Glfw.key_right_bracket
    | GRAVE_ACCENT -> Stubs.Glfw.key_grave_accent
    | WORLD_1 -> Stubs.Glfw.key_world_1
    | WORLD_2 -> Stubs.Glfw.key_world_2
    | ESCAPE -> Stubs.Glfw.key_escape
    | ENTER -> Stubs.Glfw.key_enter
    | TAB -> Stubs.Glfw.key_tab
    | BACKSPACE -> Stubs.Glfw.key_backspace
    | INSERT -> Stubs.Glfw.key_insert
    | DELETE -> Stubs.Glfw.key_delete
    | RIGHT -> Stubs.Glfw.key_right
    | LEFT -> Stubs.Glfw.key_left
    | DOWN -> Stubs.Glfw.key_down
    | UP -> Stubs.Glfw.key_up
    | PAGE_UP -> Stubs.Glfw.key_page_up
    | PAGE_DOWN -> Stubs.Glfw.key_page_down
    | HOME -> Stubs.Glfw.key_home
    | END -> Stubs.Glfw.key_end
    | CAPS_LOCK -> Stubs.Glfw.key_caps_lock
    | SCROLL_LOCK -> Stubs.Glfw.key_scroll_lock
    | NUM_LOCK -> Stubs.Glfw.key_num_lock
    | PRINT_SCREEN -> Stubs.Glfw.key_print_screen
    | PAUSE -> Stubs.Glfw.key_pause
    | F1 -> Stubs.Glfw.key_f1
    | F2 -> Stubs.Glfw.key_f2
    | F3 -> Stubs.Glfw.key_f3
    | F4 -> Stubs.Glfw.key_f4
    | F5 -> Stubs.Glfw.key_f5
    | F6 -> Stubs.Glfw.key_f6
    | F7 -> Stubs.Glfw.key_f7
    | F8 -> Stubs.Glfw.key_f8
    | F9 -> Stubs.Glfw.key_f9
    | F10 -> Stubs.Glfw.key_f10
    | F11 -> Stubs.Glfw.key_f11
    | F12 -> Stubs.Glfw.key_f12
    | F13 -> Stubs.Glfw.key_f13
    | F14 -> Stubs.Glfw.key_f14
    | F15 -> Stubs.Glfw.key_f15
    | F16 -> Stubs.Glfw.key_f16
    | F17 -> Stubs.Glfw.key_f17
    | F18 -> Stubs.Glfw.key_f18
    | F19 -> Stubs.Glfw.key_f19
    | F20 -> Stubs.Glfw.key_f20
    | F21 -> Stubs.Glfw.key_f21
    | F22 -> Stubs.Glfw.key_f22
    | F23 -> Stubs.Glfw.key_f23
    | F24 -> Stubs.Glfw.key_f24
    | F25 -> Stubs.Glfw.key_f25
    | KP_0 -> Stubs.Glfw.key_kp_0
    | KP_1 -> Stubs.Glfw.key_kp_1
    | KP_2 -> Stubs.Glfw.key_kp_2
    | KP_3 -> Stubs.Glfw.key_kp_3
    | KP_4 -> Stubs.Glfw.key_kp_4
    | KP_5 -> Stubs.Glfw.key_kp_5
    | KP_6 -> Stubs.Glfw.key_kp_6
    | KP_7 -> Stubs.Glfw.key_kp_7
    | KP_8 -> Stubs.Glfw.key_kp_8
    | KP_9 -> Stubs.Glfw.key_kp_9
    | KP_DECIMAL -> Stubs.Glfw.key_kp_decimal
    | KP_DIVIDE -> Stubs.Glfw.key_kp_divide
    | KP_MULTIPLY -> Stubs.Glfw.key_kp_multiply
    | KP_SUBTRACT -> Stubs.Glfw.key_kp_subtract
    | KP_ADD -> Stubs.Glfw.key_kp_add
    | KP_ENTER -> Stubs.Glfw.key_kp_enter
    | KP_EQUAL -> Stubs.Glfw.key_kp_equal
    | LEFT_SHIFT -> Stubs.Glfw.key_left_shift
    | LEFT_CONTROL -> Stubs.Glfw.key_left_control
    | LEFT_ALT -> Stubs.Glfw.key_left_alt
    | LEFT_SUPER -> Stubs.Glfw.key_left_super
    | RIGHT_SHIFT -> Stubs.Glfw.key_right_shift
    | RIGHT_CONTROL -> Stubs.Glfw.key_right_control
    | RIGHT_ALT -> Stubs.Glfw.key_right_alt
    | RIGHT_SUPER -> Stubs.Glfw.key_right_super
    | MENU -> Stubs.Glfw.key_menu
end

module ModifierKeys = struct
  type t = Shift | Control | Alt | Super | CapsLock | NumLock
  [@@deriving show]

  let of_glfw = function
    | x when x = Stubs.Glfw.mod_shift -> Shift
    | x when x = Stubs.Glfw.mod_control -> Control
    | x when x = Stubs.Glfw.mod_alt -> Alt
    | x when x = Stubs.Glfw.mod_super -> Super
    | x when x = Stubs.Glfw.mod_caps_lock -> CapsLock
    | x when x = Stubs.Glfw.mod_num_lock -> NumLock
    | _ -> assert false

  let to_glfw = function
    | Shift -> Stubs.Glfw.mod_shift
    | Control -> Stubs.Glfw.mod_control
    | Alt -> Stubs.Glfw.mod_alt
    | Super -> Stubs.Glfw.mod_super
    | CapsLock -> Stubs.Glfw.mod_caps_lock
    | NumLock -> Stubs.Glfw.mod_num_lock

  let of_many mods =
    if mods = 0 then
      []
    else
      let add_mod glfw_mod mod_ l =
        if mods land glfw_mod <> 0 then
          mod_ :: l
        else
          l
      in
      let res =
        []
        |> add_mod Stubs.Glfw.mod_shift Shift
        |> add_mod Stubs.Glfw.mod_control Control
        |> add_mod Stubs.Glfw.mod_alt Alt
        |> add_mod Stubs.Glfw.mod_super Super
        |> add_mod Stubs.Glfw.mod_caps_lock CapsLock
        |> add_mod Stubs.Glfw.mod_num_lock NumLock
      in
      res
end

module MouseButtons = struct
  type t =
    | Button1
    | Button2
    | Button3
    | Button4
    | Button5
    | Button6
    | Button7
    | Button8
  [@@deriving show]

  let of_glfw = function
    | x when x = Stubs.Glfw.mouse_button_1 -> Button1
    | x when x = Stubs.Glfw.mouse_button_2 -> Button2
    | x when x = Stubs.Glfw.mouse_button_3 -> Button3
    | x when x = Stubs.Glfw.mouse_button_4 -> Button4
    | x when x = Stubs.Glfw.mouse_button_5 -> Button5
    | x when x = Stubs.Glfw.mouse_button_6 -> Button6
    | x when x = Stubs.Glfw.mouse_button_7 -> Button7
    | x when x = Stubs.Glfw.mouse_button_8 -> Button8
    | _ -> assert false

  let to_glfw = function
    | Button1 -> Stubs.Glfw.mouse_button_1
    | Button2 -> Stubs.Glfw.mouse_button_2
    | Button3 -> Stubs.Glfw.mouse_button_3
    | Button4 -> Stubs.Glfw.mouse_button_4
    | Button5 -> Stubs.Glfw.mouse_button_5
    | Button6 -> Stubs.Glfw.mouse_button_6
    | Button7 -> Stubs.Glfw.mouse_button_7
    | Button8 -> Stubs.Glfw.mouse_button_8
end

module WindowClose = struct
  type t = unit [@@deriving show]

  let create () = ()
end

module WindowResize = struct
  type t = { width : int; height : int } [@@deriving show]

  let create ~width ~height = { width; height }
  let width e = e.width
  let height e = e.height
end

module WindowFocus = struct
  type t = unit [@@deriving show]

  let create () = ()
end

module WindowFocusLost = struct
  type t = unit [@@deriving show]

  let create () = ()
end

module WindowMoved = struct
  type t = { x : int; y : int } [@@deriving show]

  let create ~x ~y = { x; y }
  let x e = e.x
  let y e = e.y
end

module AppTick = struct
  type t = unit event [@@deriving show]

  let create () = ()
end

module AppRender = struct
  type t = unit event [@@deriving show]

  let create () = ()
end

module AppUpdate = struct
  type t = unit event [@@deriving show]

  let create () = ()
end

module KeyPressed = struct
  type t = { key : Keys.t; modifiers : ModifierKeys.t list } [@@deriving show]

  let create ~key ~modifiers = { key; modifiers }
  let key e = e.key
  let modifiers e = e.modifiers
end

module KeyRepeated = struct
  type t = { key : Keys.t; modifiers : ModifierKeys.t list } [@@deriving show]

  let create ~key ~modifiers = { key; modifiers }
  let key e = e.key
  let modifiers e = e.modifiers
end

module KeyReleased = struct
  type t = { key : Keys.t; modifiers : ModifierKeys.t list } [@@deriving show]

  let create ~key ~modifiers = { key; modifiers }
  let key e = e.key
  let modifiers e = e.modifiers
end

module MouseButtonPressed = struct
  type t = { button : MouseButtons.t; modifiers : ModifierKeys.t list }
  [@@deriving show]

  let create ~button ~modifiers = { button; modifiers }
  let button e = e.button
  let modifiers e = e.modifiers
end

module MouseButtonReleased = struct
  type t = { button : MouseButtons.t; modifiers : ModifierKeys.t list }
  [@@deriving show]

  let create ~button ~modifiers = { button; modifiers }
  let button e = e.button
  let modifiers e = e.modifiers
end

module MouseMoved = struct
  type t = { x : float; y : float } [@@deriving show]

  let create ~x ~y = { x; y }
  let x e = e.x
  let y e = e.y
end

module MouseScrolled = struct
  type t = { x_offset : float; y_offset : float } [@@deriving show]

  let create ~x_offset ~y_offset = { x_offset; y_offset }
  let x_offset e = e.x_offset
  let y_offset e = e.y_offset
end

type application_event = [ `Application ] [@@deriving show]
type input_event = [ `Input ] [@@deriving show]
type keyboard_event = [ `Keyboard ] [@@deriving show]
type mouse_event = [ `Mouse ] [@@deriving show]
type mouse_button_event = [ `MouseButton ] [@@deriving show]

type any_event =
  [ application_event
  | input_event
  | keyboard_event
  | mouse_event
  | mouse_button_event ]
[@@deriving show]

type _ t =
  | WindowClose : WindowClose.t -> [> application_event ] t
  | WindowResize : WindowResize.t -> [> application_event ] t
  | WindowFocus : WindowFocus.t -> [> application_event ] t
  | WindowFocusLost : WindowFocusLost.t -> [> application_event ] t
  | WindowMoved : WindowMoved.t -> [> application_event ] t
  | AppTick : AppTick.t -> [> application_event ] t
  | AppUpdate : AppUpdate.t -> [> application_event ] t
  | AppRender : AppRender.t -> [> application_event ] t
  | KeyPressed : KeyPressed.t -> [> input_event | keyboard_event ] t
  | KeyRepeated : KeyRepeated.t -> [> input_event | keyboard_event ] t
  | KeyReleased : KeyReleased.t -> [> input_event | keyboard_event ] t
  | MouseButtonPressed :
      MouseButtonPressed.t
      -> [> input_event | mouse_button_event | mouse_event ] t
  | MouseButtonReleased :
      MouseButtonReleased.t
      -> [> input_event | mouse_button_event | mouse_event ] t
  | MouseMoved : MouseMoved.t -> [> input_event | mouse_event ] t
  | MouseScrolled : MouseScrolled.t -> [> input_event | mouse_event ] t
[@@deriving show]

let create e = Live e
let bind f : 'a event -> 'b event = function Live e -> f e | Handled v -> f v
let map f = function Live e -> Live (f e) | Handled v -> Handled v
let handle f = function Live e -> Handled (f e) | Handled v -> Handled v

module Syntax = struct
  let ( let* ) = bind
  let ( let+ ) = map
  let ( let| ) = handle
  let ( >>= ) = bind
  let ( >>| ) = handle
  let ( *> ) = map
  let ( <* ) e f = map f e
end
