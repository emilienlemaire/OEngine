open Bigarray
open Math

let pp_mat4 fmt mat =
  let arr = array2_of_genarray mat in
  Format.fprintf fmt "[@[%f@ %f@ %f@ %f@\n%f@ %f@ %f@ %f@\n%f@ %f@ %f@ %f@\n%f@ %f@ %f@ %f@]]@\n"
    arr.{0, 0}
    arr.{0, 1}
    arr.{0, 2}
    arr.{0, 3}
    arr.{1, 0}
    arr.{1, 1}
    arr.{1, 2}
    arr.{1, 3}
    arr.{2, 0}
    arr.{2, 1}
    arr.{2, 2}
    arr.{2, 3}
    arr.{3, 0}
    arr.{3, 1}
    arr.{3, 2}
    arr.{3, 3}

let%expect_test "mat4_id" =
  let id = Mat4x4.id Base.Float32_elt in
  Format.printf "%a@\n" pp_mat4 id;
  [%expect
    {|
    [1.000000 0.000000 0.000000 0.000000
     0.000000 1.000000 0.000000 0.000000
     0.000000 0.000000 1.000000 0.000000
     0.000000 0.000000 0.000000 1.000000]
    |}]

let%expect_test "mat4_id*id" =
  let id = Mat4x4.id Base.Float32_elt in
  let mul = Mat4x4.mul id id in
  Format.printf "%a@\n" pp_mat4 mul;
  [%expect
    {|
    [1.000000 0.000000 0.000000 0.000000
     0.000000 1.000000 0.000000 0.000000
     0.000000 0.000000 1.000000 0.000000
     0.000000 0.000000 0.000000 1.000000]
    |}]

let%expect_test "mat4*id" =
  let id = Mat4x4.id Base.Float32_elt in
  let m = Mat4x4.init Base.Float32_elt (fun i j -> (10. *. float_of_int i) +. float_of_int j) in
  let mul = Mat4x4.mul id m in
  Format.printf "%a@\n" pp_mat4 mul;
  [%expect
    {|
    [0.000000 1.000000 2.000000 3.000000
     10.000000 11.000000 12.000000 13.000000
     20.000000 21.000000 22.000000 23.000000
     30.000000 31.000000 32.000000 33.000000]
    |}]

let%expect_test "ortho" =
  Format.printf "%a@\n" pp_mat4
  @@ Mat4x4.ortho_near_far Base.Float32_elt (-1.6) 1.6 (-0.9) 0.9 (-1.) 1.;
  [%expect
    {|
    [0.625000 0.000000 0.000000 0.000000
     0.000000 1.111111 0.000000 0.000000
     0.000000 0.000000 -1.000000 0.000000
     -0.000000 -0.000000 -0.000000 1.000000]
    |}]

let%expect_test "ortho_translate" =
  let m =
    Mat4x4.translate (Mat4x4.id Base.Float32_elt) (Vec3.of_scalars Base.Float32_elt 0.5 0.5 0.)
  in
  Format.printf "%a@\n" pp_mat4 m;
  [%expect
    {|
    [1.000000 0.000000 0.000000 0.000000
     0.000000 1.000000 0.000000 0.000000
     0.000000 0.000000 1.000000 0.000000
     0.500000 0.500000 0.000000 1.000000]
    |}]

let%expect_test "rotate" =
  let m =
    Mat4x4.rotate (Mat4x4.id Base.Float32_elt) (Base.radians 45.0)
      (Vec3.of_scalars Base.Float32_elt 0. 0. 1.)
  in
  Format.printf "%a@\n" pp_mat4 m;
  [%expect
    {|
    [0.707107 0.707107 0.000000 0.000000
     -0.707107 0.707107 0.000000 0.000000
     0.000000 0.000000 1.000000 0.000000
     0.000000 0.000000 0.000000 1.000000]
    |}]

let%expect_test "transform" =
  let trans =
    Mat4x4.translate (Mat4x4.id Base.Float32_elt) (Vec3.of_scalars Base.Float32_elt 0.5 0.5 0.)
  in
  let rot =
    Mat4x4.rotate (Mat4x4.id Base.Float32_elt) (Base.radians 45.0)
      (Vec3.of_scalars Base.Float32_elt 0. 0. 1.)
  in
  let m = Mat4x4.mul trans rot in
  Format.printf "%a@\n" pp_mat4 m;
  [%expect
    {|
    [0.707107 0.707107 0.000000 0.000000
     -0.707107 0.707107 0.000000 0.000000
     0.000000 0.000000 1.000000 0.000000
     0.500000 0.500000 0.000000 1.000000]
    |}]

let%expect_test "view_matrix" =
  let trans =
    Mat4x4.translate (Mat4x4.id Base.Float32_elt) (Vec3.of_scalars Base.Float32_elt 0.5 0.5 0.)
  in
  let rot =
    Mat4x4.rotate (Mat4x4.id Base.Float32_elt) (Base.radians 45.0)
      (Vec3.of_scalars Base.Float32_elt 0. 0. 1.)
  in
  let tranform = Mat4x4.mul trans rot in
  let res = Mat4x4.inverse tranform in
  Format.printf "%a@\n" pp_mat4 res;
  [%expect
    {|
    [0.707107 -0.707107 0.000000 -0.000000
     0.707107 0.707107 -0.000000 0.000000
     -0.000000 -0.000000 1.000000 -0.000000
     -0.707107 0.000000 -0.000000 1.000000]
    |}]

let%expect_test "view_projection_matrix" =
  let projection_matrix = Mat4x4.ortho_near_far Base.Float32_elt (-1.6) 1.6 (-0.9) 0.9 (-1.) 1. in
  let rotation = 45. in
  let position = Vec3.of_scalars Base.Float32_elt 0.5 0.5 0. in
  let transform =
    Mat4x4.mul
      (Mat4x4.translate (Mat4x4.id Base.Float32_elt) position)
      (Mat4x4.rotate (Mat4x4.id Base.Float32_elt) (Base.radians rotation)
         (Vec3.of_scalars Base.Float32_elt 0. 0. 1.) )
  in
  let view_matrix = Mat4x4.inverse transform in
  let view_projection_matrix = Mat4x4.mul projection_matrix view_matrix in
  Format.printf "%a@\n" pp_mat4 view_projection_matrix;
  [%expect
    {|
    [0.441942 -0.785674 0.000000 0.000000
     0.441942 0.785674 0.000000 0.000000
     0.000000 0.000000 -1.000000 0.000000
     -0.441942 0.000000 0.000000 1.000000]
    |}]
