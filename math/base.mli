open Bigarray

type ('a, 'b) elt =
  | Int8_signed_elt : (int, int8_signed_elt) elt
  | Int8_unsigned_elt : (int, int8_unsigned_elt) elt
  | Int16_signed_elt : (int, int16_signed_elt) elt
  | Int16_unsigned_elt : (int, int16_unsigned_elt) elt
  | Int32_elt : (int32, int32_elt) elt
  | Int64_elt : (int64, int64_elt) elt
  | Float32_elt : (float, float32_elt) elt
  | Float64_elt : (float, float64_elt) elt

type 'a binop = 'a -> 'a -> 'a

val kind : ('a, 'b) elt -> ('a, 'b) Bigarray.kind

val elt_of_kind : ('a, 'b) Bigarray.kind -> ('a, 'b) elt

val add : ('a, 'b) elt -> 'a binop

val sub : ('a, 'b) elt -> 'a binop

val mul : ('a, 'b) elt -> 'a binop

val div : ('a, 'b) elt -> 'a binop

val neg : ('a, 'b) elt -> 'a -> 'a

val zero : ('a, 'b) elt -> 'a

val one : ('a, 'b) elt -> 'a

val of_float : ('a, 'b) elt -> float -> 'a

val to_float : ('a, 'b) elt -> 'a -> float

val sqrt : (float, 'b) elt -> float -> float

val inv_sqrt : (float, 'b) elt -> float -> float

val radians : float -> float

val degrees : float -> float
