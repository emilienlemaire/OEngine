open Bigarray

(** As in glsl, matrix are column major ordered in the memory *)

(** Note: as of now, only square matrices are handled *)

type ('a, 'b) t = ('a, 'b, c_layout) Genarray.t

let empty elt dim = Genarray.create (Base.kind elt) c_layout [| dim; dim |]

let zeros elt dim = Genarray.init (Base.kind elt) c_layout [| dim; dim |] (fun _ -> Base.zero elt)

let ones elt dim = Genarray.init (Base.kind elt) c_layout [| dim; dim |] (fun _ -> Base.one elt)

let share arr =
  let dims = Genarray.dims arr in
  (dims.(0), dims.(1))

let init elt dim f =
  Genarray.init (Base.kind elt) c_layout [| dim; dim |] (fun arr -> f arr.(0) arr.(1))

let scalar_mul s arr =
  let n = (Genarray.dims arr).(0) in
  let elt = Base.elt_of_kind @@ Genarray.kind arr in
  let mul = Base.mul elt in
  Genarray.init (Genarray.kind arr) c_layout [| n; n |] (fun idx -> mul (Genarray.get arr idx) s)

let _mat_member_op : type a b. ((a, b) Base.elt -> a Base.binop) -> (a, b) t -> (a, b) t -> (a, b) t
    =
 fun op m1 m2 ->
  if Genarray.kind m1 <> Genarray.kind m2 then
    invalid_arg __FUNCTION__
  else
    let n1 = (Genarray.dims m1).(0) in
    let n2 = (Genarray.dims m2).(0) in
    if n1 <> n2 then
      invalid_arg __FUNCTION__
    else
      let elt = Base.elt_of_kind @@ Genarray.kind m1 in
      let op_ = op elt in
      Genarray.init (Genarray.kind m1) c_layout [| n1; n1 |] (fun i ->
          op_ (Genarray.get m1 i) (Genarray.get m2 i) )
[@@inline]

type ('a, 'b) mat_op = ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t

let add : type a b. (a, b) mat_op = fun m1 m2 -> _mat_member_op Base.add m1 m2

let sub : type a b. (a, b) mat_op = fun m1 m2 -> _mat_member_op Base.sub m1 m2

let div : type a b. (a, b) mat_op = fun m1 m2 -> _mat_member_op Base.div m1 m2

let mul : type a b. (a, b) mat_op =
 fun m1 m2 ->
  if Genarray.kind m1 <> Genarray.kind m2 then
    invalid_arg __FUNCTION__
  else
    let n1 = (Genarray.dims m1).(0) in
    let n2 = (Genarray.dims m2).(0) in
    if n1 <> n2 then
      invalid_arg __FUNCTION__
    else
      let elt = Base.elt_of_kind @@ Genarray.kind m1 in
      let ( * ) = Base.mul elt in
      let ( + ) = Base.add elt in
      let arr1 = array2_of_genarray m1 in
      let arr2 = array2_of_genarray m2 in
      let rec c i j k acc =
        if k = n1 then
          acc
        else
          let a = Array2.unsafe_get arr1 i k in
          let b = Array2.unsafe_get arr2 k j in
          let acc = acc + (a * b) in
          c i j (Int.add k 1) acc
      in
      Genarray.init (Base.kind elt) c_layout [| n1; n1 |] (fun arr ->
          c arr.(0) arr.(1) 0 (Base.one elt) )

let _set_unit : type a b. (a, b) t -> int * int -> a -> unit =
 fun mat (i, j) v -> Genarray.set mat [| i; j |] v

let set : type a b. (a, b) t -> int -> int -> a -> (a, b) t =
 fun mat i j v ->
  Genarray.set mat [| i; j |] v;
  mat

let get : type a b. (a, b) t -> int * int -> a = fun mat (i, j) -> Genarray.get mat [| i; j |]

module Syntax = struct
  let ( !+! ) = add

  let ( !-! ) = sub

  let ( !*! ) = mul

  let ( !/! ) = div

  let ( *! ) = scalar_mul

  let ( .![] ) = get

  let ( .![]<- ) = _set_unit
end
