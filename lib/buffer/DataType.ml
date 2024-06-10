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
