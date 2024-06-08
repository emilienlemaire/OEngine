open Bigarray

type ('a, 'b) t = ('a, 'b, c_layout) Genarray.t

let empty elt = Genarray.create (Base.kind elt) c_layout [| 4; 4 |]

let zeros elt = Genarray.init (Base.kind elt) c_layout [| 4; 4 |] (fun _ -> Base.zero elt)

let ones elt = Genarray.init (Base.kind elt) c_layout [| 4; 4 |] (fun _ -> Base.one elt)

let share _arr = (4, 4)

let init elt f = Genarray.init (Base.kind elt) c_layout [| 4; 4 |] (fun arr -> f arr.(0) arr.(1))

let id elt =
  let arr = array2_of_genarray @@ zeros elt in
  let one = Base.one elt in
  arr.{0, 0} <- one;
  arr.{1, 1} <- one;
  arr.{2, 2} <- one;
  arr.{3, 3} <- one;
  genarray_of_array2 arr

let diag elt v =
  let arr = array2_of_genarray @@ zeros elt in
  arr.{0, 0} <- v;
  arr.{1, 1} <- v;
  arr.{2, 2} <- v;
  arr.{3, 3} <- v;
  genarray_of_array2 arr

let scalar_mul s arr =
  let elt = Base.elt_of_kind @@ Genarray.kind arr in
  let mul = Base.mul elt in
  Genarray.init (Genarray.kind arr) c_layout [| 4; 4 |] (fun idx -> mul (Genarray.get arr idx) s)

let _mat_member_op : type a b. ((a, b) Base.elt -> a Base.binop) -> (a, b) t -> (a, b) t -> (a, b) t
    =
 fun op m1 m2 ->
  let elt = Base.elt_of_kind @@ Genarray.kind m1 in
  let op_ = op elt in
  Genarray.init (Genarray.kind m1) c_layout [| 4; 4 |] (fun i ->
      op_ (Genarray.get m1 i) (Genarray.get m2 i) )
[@@inline]

type ('a, 'b) mat_op = ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t

let add : type a b. (a, b) mat_op = fun m1 m2 -> _mat_member_op Base.add m1 m2

let sub : type a b. (a, b) mat_op = fun m1 m2 -> _mat_member_op Base.sub m1 m2

let div : type a b. (a, b) mat_op = fun m1 m2 -> _mat_member_op Base.div m1 m2

let mul : type a b. (a, b) mat_op =
 fun m1 m2 ->
  let elt = Base.elt_of_kind @@ Genarray.kind m1 in
  let ( * ) = Base.mul elt in
  let ( + ) = Base.add elt in
  let arr1 = array2_of_genarray m1 in
  let arr2 = array2_of_genarray m2 in
  let rec c i j k acc =
    if k = 4 then
      acc
    else
      let a = Array2.unsafe_get arr1 i k in
      let b = Array2.unsafe_get arr2 k j in
      let acc = acc + (a * b) in
      c i j (Int.add k 1) acc
  in
  Genarray.init (Base.kind elt) c_layout [| 4; 4 |] (fun arr -> c arr.(0) arr.(1) 0 (Base.one elt))

let _set_unit : type a b. (a, b) t -> int * int -> a -> unit =
 fun mat (i, j) v -> Genarray.set mat [| i; j |] v

let set : type a b. (a, b) t -> int -> int -> a -> (a, b) t =
 fun mat i j v ->
  Genarray.set mat [| i; j |] v;
  mat

let get : type a b. (a, b) t -> int * int -> a = fun mat (i, j) -> Genarray.get mat [| i; j |]

let get_col : type a b. (a, b) t -> int -> (a, b) Vec4.t =
 fun mat i ->
  let slice = Genarray.slice_left mat [| i |] in
  Vec4.of_genarray slice

module Syntax = struct
  let ( !+! ) = add

  let ( !-! ) = sub

  let ( !*! ) = mul

  let ( !/! ) = div

  let ( *! ) = scalar_mul

  let ( .![] ) = get

  let ( .![]<- ) = _set_unit
end

open Syntax

let _set_vec4 : type a b. (a, b) t -> int -> (a, b) Vec4.t -> (a, b) t =
 fun mat idx vec ->
  let res = Genarray.create (Genarray.kind mat) c_layout [| 4; 4 |] in
  Genarray.blit mat res;
  let slice = Genarray.slice_left res [| idx |] in
  let gvec = Vec4.as_genarray vec in
  Genarray.blit gvec slice;
  res

let _set_vec4' : type a b. (a, b) t -> int -> (a, b) Vec4.t -> unit =
 fun mat idx vec ->
  let slice = Genarray.slice_left mat [| idx |] in
  let gvec = Vec4.as_genarray vec in
  Genarray.blit gvec slice

let _set_vec3 : type a b. (a, b) t -> int -> (a, b) Vec3.t -> (a, b) t =
 fun mat idx vec ->
  let res = Genarray.create (Genarray.kind mat) c_layout [| 4; 4 |] in
  Genarray.blit mat res;
  let slice = Genarray.slice_left res [| idx |] in
  let sub = Genarray.sub_left slice 0 3 in
  let gvec = Vec3.as_genarray vec in
  Genarray.blit gvec sub;
  res

let _set_vec2 : type a b. (a, b) t -> int -> (a, b) Vec2.t -> (a, b) t =
 fun mat idx vec ->
  let res = Genarray.create (Genarray.kind mat) c_layout [| 4; 4 |] in
  Genarray.blit mat res;
  let slice = Genarray.slice_left res [| idx |] in
  let sub = Genarray.sub_left slice 0 2 in
  let gvec = Vec2.as_genarray vec in
  Genarray.blit gvec sub;
  res

let ortho : type a b. (a, b) Base.elt -> a -> a -> a -> a -> (a, b) t =
 fun elt left right bottom top ->
  let ( + ) = Base.add elt in
  let ( - ) = Base.sub elt in
  let ( ~- ) = Base.neg elt in
  let ( / ) = Base.div elt in
  let two = Base.one elt + Base.one elt in
  let m = ones elt in
  m.![0, 0] <- two / (right - left);
  m.![1, 1] <- two / (top - bottom);
  m.![2, 2] <- ~-(Base.one elt);
  m.![3, 0] <- ~-(right + left) / (right - left);
  m.![3, 1] <- ~-(top + bottom) / (top - bottom);
  m

let ortho_near_far : type a b. (a, b) Base.elt -> a -> a -> a -> a -> a -> a -> (a, b) t =
 fun elt left right bottom top near far ->
  let ( + ) = Base.add elt in
  let ( - ) = Base.sub elt in
  let ( ~- ) = Base.neg elt in
  let ( / ) = Base.div elt in
  let one = Base.one elt in
  let two = one + one in
  let m = ones elt in
  m.![0, 0] <- two / (right - left);
  m.![1, 1] <- two / (top - bottom);
  m.![2, 2] <- one / (far - near);
  m.![3, 0] <- ~-(right + left) / (right - left);
  m.![3, 1] <- ~-(top + bottom) / (top - bottom);
  m.![3, 2] <- ~-near / (far - near);
  m

let translate : type a b. (a, b) t -> (a, b) Vec3.t -> (a, b) t =
 fun mat vec ->
  let res = Genarray.create (Genarray.kind mat) c_layout [| 4; 4 |] in
  Genarray.blit mat res;
  let translate =
    let open Vec4.Syntax in
    let ( .^[] ) = Vec3.Syntax.( .^[] ) in
    let m0 = get_col mat 0 in
    let m1 = get_col mat 1 in
    let m2 = get_col mat 2 in
    let m3 = get_col mat 3 in
    (vec.^[0] *^ m0) ^+^ (vec.^[1] *^ m1) ^+^ (vec.^[2] *^ m2) ^+^ m3
  in
  _set_vec4 res 3 translate

let rotate : type b. (float, b) t -> float -> (float, b) Vec3.t -> (float, b) t =
 fun mat angle vec ->
  let elt = Base.elt_of_kind @@ Genarray.kind mat in
  let to_float = Base.to_float elt in
  let of_float = Base.of_float elt in
  let c = of_float @@ Float.cos (to_float angle) in
  let s = of_float @@ Float.sin (to_float angle) in
  let axis = Vec3.normalize vec in
  let one = Base.one elt in
  let ( - ) = Base.sub elt in
  let ( *^ ) = Vec3.Syntax.( *^ ) in
  let ( + ) = Base.add elt in
  let ( * ) = Base.mul elt in
  let ( .^[] ) = Vec3.get in
  let temp = (one - c) *^ axis in
  let rotate = zeros elt in
  (* Rotate[0][0] = c + temp[0] * axis[0]; *)
  rotate.![0, 0] <- c + (temp.^[0] * axis.^[0]);
  (* Rotate[0][1] = temp[0] * axis[1] + s * axis[2]; *)
  rotate.![0, 1] <- (temp.^[0] * axis.^[1]) + (s * axis.^[2]);
  (* Rotate[0][2] = temp[0] * axis[2] - s * axis[1]; *)
  rotate.![0, 2] <- (temp.^[0] * axis.^[2]) - (s * axis.^[1]);
  (* Rotate[1][0] = temp[1] * axis[0] - s * axis[2]; *)
  rotate.![1, 0] <- (temp.^[1] * axis.^[0]) - (s * axis.^[2]);
  (* Rotate[1][1] = c + temp[1] * axis[1]; *)
  rotate.![1, 1] <- c + (temp.^[1] * axis.^[1]);
  (* Rotate[1][2] = temp[1] * axis[2] + s * axis[0]; *)
  rotate.![1, 2] <- (temp.^[1] * axis.^[2]) + (s * axis.^[0]);

  (* Rotate[2][0] = temp[2] * axis[0] + s * axis[1]; *)
  rotate.![2, 0] <- (temp.^[2] * axis.^[0]) + (s * axis.^[1]);
  (* Rotate[2][1] = temp[2] * axis[1] - s * axis[0]; *)
  rotate.![2, 1] <- (temp.^[2] * axis.^[1]) - (s * axis.^[0]);
  (* Rotate[2][2] = c + temp[2] * axis[2]; *)
  rotate.![2, 2] <- c + (temp.^[2] * axis.^[2]);

  let result = zeros elt in
  let m0 = get_col mat 0 in
  let m1 = get_col mat 1 in
  let m2 = get_col mat 2 in
  let m3 = get_col mat 3 in
  let open Vec4.Syntax in
  (* Result[0] = m[0] * Rotate[0][0] + m[1] * Rotate[0][1] + m[2] * Rotate[0][2]; *)
  _set_vec4' result 0 ((rotate.![0, 0] *^ m0) ^+^ (rotate.![0, 1] *^ m1) ^+^ (rotate.![0, 2] *^ m2));
  (* Result[1] = m[0] * Rotate[1][0] + m[1] * Rotate[1][1] + m[2] * Rotate[1][2]; *)
  _set_vec4' result 1 ((rotate.![1, 0] *^ m0) ^+^ (rotate.![1, 1] *^ m1) ^+^ (rotate.![1, 2] *^ m2));
  (* Result[2] = m[0] * Rotate[2][0] + m[1] * Rotate[2][1] + m[2] * Rotate[2][2]; *)
  _set_vec4' result 2 ((rotate.![2, 0] *^ m0) ^+^ (rotate.![2, 1] *^ m1) ^+^ (rotate.![2, 2] *^ m2));
  _set_vec4' result 3 m3;
  result

let inverse : type a b. (a, b) t -> (a, b) t =
 fun m ->
  let elt = Base.elt_of_kind @@ Genarray.kind m in
  let ( * ) = Base.mul elt in
  let ( / ) = Base.div elt in
  let ( - ) = Base.sub elt in
  let ( + ) = Base.add elt in
  let coef00 = (m.![2, 2] * m.![3, 3]) - (m.![3, 2] * m.![2, 3]) in
  let coef02 = (m.![1, 2] * m.![3, 3]) - (m.![3, 2] * m.![1, 3]) in
  let coef03 = (m.![1, 2] * m.![2, 3]) - (m.![2, 2] * m.![1, 3]) in
  let coef04 = (m.![2, 1] * m.![3, 3]) - (m.![3, 1] * m.![2, 3]) in
  let coef06 = (m.![1, 1] * m.![3, 3]) - (m.![3, 1] * m.![1, 3]) in
  let coef07 = (m.![1, 1] * m.![2, 3]) - (m.![2, 1] * m.![1, 3]) in
  let coef08 = (m.![2, 1] * m.![3, 2]) - (m.![3, 1] * m.![2, 2]) in
  let coef10 = (m.![1, 1] * m.![3, 2]) - (m.![3, 1] * m.![1, 2]) in
  let coef11 = (m.![1, 1] * m.![2, 2]) - (m.![2, 1] * m.![1, 2]) in
  let coef12 = (m.![2, 0] * m.![3, 3]) - (m.![3, 0] * m.![2, 3]) in
  let coef14 = (m.![1, 0] * m.![3, 3]) - (m.![3, 0] * m.![1, 3]) in
  let coef15 = (m.![1, 0] * m.![2, 3]) - (m.![2, 0] * m.![1, 3]) in
  let coef16 = (m.![2, 0] * m.![3, 2]) - (m.![3, 0] * m.![2, 2]) in
  let coef18 = (m.![1, 0] * m.![3, 2]) - (m.![3, 0] * m.![1, 2]) in
  let coef19 = (m.![1, 0] * m.![2, 2]) - (m.![2, 0] * m.![1, 2]) in
  let coef20 = (m.![2, 0] * m.![3, 1]) - (m.![3, 0] * m.![2, 1]) in
  let coef22 = (m.![1, 0] * m.![3, 1]) - (m.![3, 0] * m.![1, 1]) in
  let coef23 = (m.![1, 0] * m.![2, 1]) - (m.![2, 0] * m.![1, 1]) in
  let fac0 = Vec4.of_scalars elt coef00 coef00 coef02 coef03 in
  let fac1 = Vec4.of_scalars elt coef04 coef04 coef06 coef07 in
  let fac2 = Vec4.of_scalars elt coef08 coef08 coef10 coef11 in
  let fac3 = Vec4.of_scalars elt coef12 coef12 coef14 coef15 in
  let fac4 = Vec4.of_scalars elt coef16 coef16 coef18 coef19 in
  let fac5 = Vec4.of_scalars elt coef20 coef20 coef22 coef23 in
  let vec0 = Vec4.of_scalars elt m.![1, 0] m.![0, 0] m.![0, 0] m.![0, 0] in
  let vec1 = Vec4.of_scalars elt m.![1, 1] m.![0, 1] m.![0, 1] m.![0, 1] in
  let vec2 = Vec4.of_scalars elt m.![1, 2] m.![0, 2] m.![0, 2] m.![0, 2] in
  let vec3 = Vec4.of_scalars elt m.![1, 3] m.![0, 3] m.![0, 3] m.![0, 3] in
  let open Vec4.Syntax in
  let inv0 = vec1 ^*^ fac0 ^-^ vec2 ^*^ fac1 ^+^ vec3 ^*^ fac2 in
  let inv1 = vec0 ^*^ fac0 ^-^ vec2 ^*^ fac3 ^+^ vec3 ^*^ fac4 in
  let inv2 = vec0 ^*^ fac1 ^-^ vec1 ^*^ fac3 ^+^ vec3 ^*^ fac5 in
  let inv3 = vec0 ^*^ fac2 ^-^ vec1 ^*^ fac4 ^+^ vec2 ^*^ fac5 in
  let one = Base.one elt in
  let neg = Base.neg elt in
  let neg_one = neg one in
  let signa = Vec4.of_scalars elt one neg_one one neg_one in
  let signb = Vec4.of_scalars elt neg_one one neg_one one in
  let inverse = zeros elt in
  _set_vec4' inverse 0 (inv0 ^*^ signa);
  _set_vec4' inverse 1 (inv1 ^*^ signb);
  _set_vec4' inverse 2 (inv2 ^*^ signa);
  _set_vec4' inverse 3 (inv3 ^*^ signb);
  let row0 = Vec4.of_scalars elt inverse.![0, 0] inverse.![1, 0] inverse.![2, 0] inverse.![3, 0] in
  let dot0 = get_col m 0 ^*^ row0 in
  let dot1 = dot0.^[0] + dot0.^[1] + (dot0.^[2] + dot0.^[3]) in
  let one_over_determinant = one / dot1 in
  one_over_determinant *! inverse

let start : type a b. (a, b) t -> a Ctypes.ptr =
 fun mat -> Ctypes.bigarray_start Ctypes.genarray mat
