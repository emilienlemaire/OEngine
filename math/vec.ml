open Bigarray

type ('a, 'b) t = ('a, 'b, c_layout) Genarray.t

let empty elt dim = Genarray.create (Base.kind elt) c_layout [| dim |]

let zeros elt dim =
  Genarray.init (Base.kind elt) c_layout [| dim |] (fun _ -> Base.zero elt)

let ones elt dim =
  Genarray.init (Base.kind elt) c_layout [| dim |] (fun _ -> Base.one elt)

let shape arr = (Genarray.dims arr).(0)

let init elt dim f =
  genarray_of_array1 @@ Array1.init (Base.kind elt) c_layout dim f

let scalar_mul s arr =
  let n = (Genarray.dims arr).(0) in
  let arr1 = array1_of_genarray arr in
  let elt = Base.elt_of_kind @@ Genarray.kind arr in
  let mul = Base.mul elt in
  genarray_of_array1
  @@ Array1.init (Genarray.kind arr) c_layout n (fun i ->
         mul (Array1.unsafe_get arr1 i) s)

let _vec_op :
    type a b.
    ((a, b) Base.elt -> a Base.binop) -> (a, b) t -> (a, b) t -> (a, b) t =
 fun op v1 v2 ->
  if Genarray.kind v1 <> Genarray.kind v2 then
    invalid_arg "Vec.add"
  else
    let n1 = (Genarray.dims v1).(0) in
    let n2 = (Genarray.dims v2).(0) in
    if n1 <> n2 then
      invalid_arg __FUNCTION__
    else
      let arr1 = array1_of_genarray v1 in
      let arr2 = array1_of_genarray v2 in
      let elt = Base.elt_of_kind @@ Genarray.kind v1 in
      let op_ = op elt in
      genarray_of_array1
      @@ Array1.init (Genarray.kind v1) c_layout n1 (fun i ->
             op_ (Array1.unsafe_get arr1 i) (Array1.unsafe_get arr2 i))
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

let get : type a b. (a, b) t -> int -> a =
 fun vec i -> Array1.unsafe_get (array1_of_genarray vec) i

module Syntax = struct
  let ( + ) = add
  let ( - ) = sub
  let ( * ) = mul
  let ( / ) = div
  let ( !* ) = scalar_mul
  let ( .![] ) = get
  let ( .![]<- ) = _set_unit
end
