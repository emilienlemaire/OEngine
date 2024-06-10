open Math

type t =
  { projection_matrix : (float, Bigarray.float32_elt) Mat4x4.t;
    view_matrix : (float, Bigarray.float32_elt) Mat4x4.t;
    view_projection_matrix : (float, Bigarray.float32_elt) Mat4x4.t;
    position : (float, Bigarray.float32_elt) Vec3.t;
    rotation : float
  }

let make left right bottom top =
  let projection_matrix = Mat4x4.ortho_near_far Base.Float32_elt left right bottom top (-1.) 1. in
  let view_matrix = Mat4x4.ones Base.Float32_elt in
  let view_projection_matrix = Mat4x4.mul projection_matrix view_matrix in
  let position = Vec3.zeros Base.Float32_elt in
  let rotation = 0. in
  { projection_matrix; view_matrix; view_projection_matrix; position; rotation }

let recalculate_view_matrix m =
  let transform =
    Mat4x4.mul
      (Mat4x4.translate (Mat4x4.id Base.Float32_elt) m.position)
      (Mat4x4.rotate (Mat4x4.id Base.Float32_elt) (Base.radians m.rotation)
         (Vec3.of_scalars Base.Float32_elt 0. 0. 1.) )
  in
  let view_matrix = Mat4x4.inverse transform in
  let view_projection_matrix = Mat4x4.mul m.projection_matrix view_matrix in
  { m with view_matrix; view_projection_matrix }

let set_position v m = recalculate_view_matrix { m with position = v }

let position m = m.position

let set_rotation r m = recalculate_view_matrix { m with rotation = r }

let rotation m = m.rotation

let projection_matrix m = m.projection_matrix

let view_matrix m = m.view_matrix

let view_projection_matrix m =
  let dst = Mat4x4.empty Base.Float32_elt in
  Bigarray.Genarray.blit m.view_projection_matrix dst;
  dst
