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

let kind : type a b. (a, b) elt -> (a, b) Bigarray.kind = function
  | Int8_signed_elt -> Bigarray.Int8_signed
  | Int8_unsigned_elt -> Bigarray.Int8_unsigned
  | Int16_signed_elt -> Bigarray.Int16_signed
  | Int16_unsigned_elt -> Bigarray.Int16_unsigned
  | Int32_elt -> Bigarray.Int32
  | Int64_elt -> Bigarray.Int64
  | Float32_elt -> Bigarray.Float32
  | Float64_elt -> Bigarray.Float64

let elt_of_kind : type a b. (a, b) Bigarray.kind -> (a, b) elt = function
  | Bigarray.Int8_signed -> Int8_signed_elt
  | Bigarray.Int8_unsigned -> Int8_unsigned_elt
  | Bigarray.Int16_signed -> Int16_signed_elt
  | Bigarray.Int16_unsigned -> Int16_unsigned_elt
  | Bigarray.Int32 -> Int32_elt
  | Bigarray.Int64 -> Int64_elt
  | Bigarray.Float32 -> Float32_elt
  | Bigarray.Float64 -> Float64_elt
  | _ -> invalid_arg "elt_of_kind"

let add : type a b. (a, b) elt -> a binop = function
  | Int8_signed_elt -> ( + )
  | Int8_unsigned_elt -> ( + )
  | Int16_signed_elt -> ( + )
  | Int16_unsigned_elt -> ( + )
  | Int32_elt -> Int32.add
  | Int64_elt -> Int64.add
  | Float32_elt -> ( +. )
  | Float64_elt -> ( +. )

let sub : type a b. (a, b) elt -> a binop = function
  | Int8_signed_elt -> ( - )
  | Int8_unsigned_elt -> ( - )
  | Int16_signed_elt -> ( - )
  | Int16_unsigned_elt -> ( - )
  | Int32_elt -> Int32.sub
  | Int64_elt -> Int64.sub
  | Float32_elt -> ( -. )
  | Float64_elt -> ( -. )

let mul : type a b. (a, b) elt -> a binop = function
  | Int8_signed_elt -> ( * )
  | Int8_unsigned_elt -> ( * )
  | Int16_signed_elt -> ( * )
  | Int16_unsigned_elt -> ( * )
  | Int32_elt -> Int32.mul
  | Int64_elt -> Int64.mul
  | Float32_elt -> ( *. )
  | Float64_elt -> ( *. )

let div : type a b. (a, b) elt -> a binop = function
  | Int8_signed_elt -> ( / )
  | Int8_unsigned_elt -> ( / )
  | Int16_signed_elt -> ( / )
  | Int16_unsigned_elt -> ( / )
  | Int32_elt -> Int32.div
  | Int64_elt -> Int64.div
  | Float32_elt -> ( /. )
  | Float64_elt -> ( /. )

let neg : type a b. (a, b) elt -> a -> a = function
  | Int8_signed_elt -> ( ~- )
  | Int8_unsigned_elt -> ( ~- )
  | Int16_signed_elt -> ( ~- )
  | Int16_unsigned_elt -> ( ~- )
  | Int32_elt -> Int32.neg
  | Int64_elt -> Int64.neg
  | Float32_elt -> ( ~-. )
  | Float64_elt -> ( ~-. )

let zero : type a b. (a, b) elt -> a = function
  | Int8_signed_elt -> 0
  | Int8_unsigned_elt -> 0
  | Int16_signed_elt -> 0
  | Int16_unsigned_elt -> 0
  | Int32_elt -> Int32.zero
  | Int64_elt -> Int64.zero
  | Float32_elt -> 0.
  | Float64_elt -> 0.

let one : type a b. (a, b) elt -> a = function
  | Int8_signed_elt -> 1
  | Int8_unsigned_elt -> 1
  | Int16_signed_elt -> 1
  | Int16_unsigned_elt -> 1
  | Int32_elt -> Int32.one
  | Int64_elt -> Int64.one
  | Float32_elt -> 1.
  | Float64_elt -> 1.

let of_float : type a b. (a, b) elt -> float -> a = function
  | Int8_signed_elt -> int_of_float
  | Int8_unsigned_elt -> int_of_float
  | Int16_signed_elt -> int_of_float
  | Int16_unsigned_elt -> int_of_float
  | Int32_elt -> Int32.of_float
  | Int64_elt -> Int64.of_float
  | Float32_elt -> Fun.id
  | Float64_elt -> Fun.id

let to_float : type a b. (a, b) elt -> a -> float = function
  | Int8_signed_elt -> float_of_int
  | Int8_unsigned_elt -> float_of_int
  | Int16_signed_elt -> float_of_int
  | Int16_unsigned_elt -> float_of_int
  | Int32_elt -> Int32.to_float
  | Int64_elt -> Int64.to_float
  | Float32_elt -> Fun.id
  | Float64_elt -> Fun.id

let sqrt : type b. (float, b) elt -> float -> float = function _ -> sqrt

let inv_sqrt : type b. (float, b) elt -> float -> float = function
  | _ -> fun x -> 1. /. Stdlib.sqrt x

let radians degrees = degrees *. (Float.pi /. 180.)

let degrees radians = radians *. (180. /. Float.pi)
