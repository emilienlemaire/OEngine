open Bigarray

type ('a, 'b) t = ('a, 'b, c_layout) Genarray.t

let empty elt = Genarray.create (Base.kind elt) c_layout [| 2 |]

let zeros elt = Genarray.init (Base.kind elt) c_layout [| 2 |] (fun _ -> Base.zero elt)

let ones elt = Genarray.init (Base.kind elt) c_layout [| 2 |] (fun _ -> Base.one elt)

let shape _arr = 2

let init elt f = genarray_of_array1 @@ Array1.init (Base.kind elt) c_layout 2 f

let scalar_mul s arr =
  let arr1 = array1_of_genarray arr in
  let elt = Base.elt_of_kind @@ Genarray.kind arr in
  let mul = Base.mul elt in
  let arr = Array1.create (Base.kind elt) c_layout 2 in
  Array1.unsafe_set arr 0 (mul (Array1.unsafe_get arr1 0) s);
  Array1.unsafe_set arr 1 (mul (Array1.unsafe_get arr1 1) s);
  genarray_of_array1 arr

let _vec_op : type a b. ((a, b) Base.elt -> a Base.binop) -> (a, b) t -> (a, b) t -> (a, b) t =
 fun op v1 v2 ->
  if Genarray.kind v1 <> Genarray.kind v2 then
    invalid_arg __FUNCTION__
  else
    let arr1 = array1_of_genarray v1 in
    let arr2 = array1_of_genarray v2 in
    let elt = Base.elt_of_kind @@ Genarray.kind v1 in
    let op_ = op elt in
    let arr = Array1.create (Base.kind elt) c_layout 2 in
    Array1.unsafe_set arr 0 (op_ (Array1.unsafe_get arr1 0) (Array1.unsafe_get arr2 0));
    Array1.unsafe_set arr 1 (op_ (Array1.unsafe_get arr1 1) (Array1.unsafe_get arr2 1));
    genarray_of_array1 arr
[@@inline]

type ('a, 'b) vec_op = ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t

let add : type a b. (a, b) vec_op = fun v1 v2 -> _vec_op Base.add v1 v2

let sub : type a b. (a, b) vec_op = fun v1 v2 -> _vec_op Base.sub v1 v2

let mul : type a b. (a, b) vec_op = fun v1 v2 -> _vec_op Base.mul v1 v2

let div : type a b. (a, b) vec_op = fun v1 v2 -> _vec_op Base.div v1 v2

let _set_unit : type a b. (a, b) t -> int -> a -> unit =
 fun vec i v -> Array1.unsafe_set (array1_of_genarray vec) i v

let set : type a b. (a, b) t -> int -> a -> (a, b) t =
 fun vec i v ->
  Array1.unsafe_set (array1_of_genarray vec) i v;
  vec

let get : type a b. (a, b) t -> int -> a = fun vec i -> Array1.unsafe_get (array1_of_genarray vec) i

let x arr = Array1.unsafe_get (array1_of_genarray arr) 0

let y arr = Array1.unsafe_get (array1_of_genarray arr) 1

let r = x

let g = y

let s = x

let t = y

let as_genarray : type a b. (a, b) t -> (a, b, c_layout) Genarray.t = fun v -> v

module Syntax = struct
  let ( ^+^ ) = add

  let ( ^-^ ) = sub

  let ( ^*^ ) = mul

  let ( ^/^ ) = div

  let ( *^ ) = scalar_mul

  let ( .^[] ) = get

  let ( .^[]<- ) = _set_unit
end
