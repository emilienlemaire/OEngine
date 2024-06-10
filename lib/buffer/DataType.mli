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

val size_in_bytes : t -> int

val element_count : t -> int
