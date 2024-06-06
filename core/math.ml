open Bigarray

type ('a, 'b) arr = ('a, 'b, Bigarray.c_layout) Bigarray.Genarray.t

let _prepend_dims dims desired_len =
  let dims_len = Array.length dims in
  if dims_len >= desired_len then
    dims
  else
    Array.append (Array.make (desired_len - dims_len) 1) dims

let _get_broadcasted_dims dims_a dims_b =
  let len_c = Stdlib.max (Array.length dims_a) (Array.length dims_b) in
  let ext_dims_a = _prepend_dims dims_a len_c in
  let ext_dims_b = _prepend_dims dims_b len_c in
  let dims_c = Array.make len_c 0 in
  for i = 0 to len_c - 1 do
    let val_a = ext_dims_a.(i) in
    let val_b = ext_dims_b.(i) in
    if val_a = val_b then
      dims_c.(i) <- val_a
    else if val_a != 1 && val_b != 1 then
      raise
        (Invalid_argument "The arrays cannot be broadcast into the same shape")
    else
      dims_c.(i) <- Stdlib.max val_a val_b
  done;
  (ext_dims_a, ext_dims_b, dims_c)

let shape = Genarray.dims
let kind = Genarray.kind
let empty kind dims = Genarray.create kind c_layout dims

let _get_broadcasted_index ind dims =
  let num_dims = Array.length dims in
  let calc_fun i =
    let max_ind = dims.(i) in
    let ind_val = ind.(i) in
    if ind_val < max_ind then
      ind_val
    else if max_ind = 1 then
      0
    else
      raise (Invalid_argument "not broadcasted correctly")
  in
  Array.init num_dims calc_fun

let _next_index ind dims =
  let num_dims = Array.length ind in
  let p = ref (num_dims - 1) in
  let ok = ref false in
  while !p >= 0 && not !ok do
    if ind.(!p) + 1 < dims.(!p) then (
      ind.(!p) <- ind.(!p) + 1;
      ok := true)
    else (
      ind.(!p) <- 0;
      p := !p - 1)
  done;
  !ok

let _broadcasted_op ?out varr_a varr_b op_fun =
  let dims_a, dims_b, dims_c =
    _get_broadcasted_dims (shape varr_a) (shape varr_b)
  in
  let _kind = kind varr_a in
  let varr_a = reshape varr_a dims_a in
  let varr_b = reshape varr_b dims_b in
  let varr_c = match out with Some out -> out | None -> empty _kind dims_c in
  let ind = Array.make (Array.length dims_c) 0 in
  let should_stop = ref false in
  while not !should_stop do
    let ind_a = _get_broadcasted_index ind dims_a in
    let ind_b = _get_broadcasted_index ind dims_b in
    Genarray.set varr_c ind
      (op_fun (Genarray.get varr_a ind_a) (Genarray.get varr_b ind_b));
    if not (_next_index ind dims_c) then
      should_stop := true
  done;
  varr_c

module type VEC_SPEC = sig
  type t
  type elt

  val length : int
  val kind : (t, elt) Bigarray.kind
end

module Vec (S : VEC_SPEC) = struct
  type t = { storage : (S.t, S.elt, Bigarray.c_layout) Bigarray.Array1.t }

  let create f = { storage = Bigarray.Array1.init S.kind c_layout S.length f }
end

module type VEC4_SPEC = sig
  type t
  type elt

  val kind : (t, elt) Bigarray.kind
end

module Vec4 (S : VEC4_SPEC) = struct
  type t = { storage : (S.t, S.elt, Bigarray.c_layout) Bigarray.Array1.t }

  let create a b c d =
    let arr = Bigarray.Array1.create S.kind c_layout 4 in
    Bigarray.Array1.set arr 0 a;
    Bigarray.Array1.set arr 1 b;
    Bigarray.Array1.set arr 2 c;
    Bigarray.Array1.set arr 3 d;
    { storage = arr }
end

module Vec4_Float = struct
  include Vec4 (struct
    type t = float
    type elt = float64_elt

    let kind = Float64
  end)

  let ( * ) a b = _broadcasted_op a b Float.mul
end
