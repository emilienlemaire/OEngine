open Math

type t

val make : float -> float -> float -> float -> t

val set_position : (float, Bigarray.float32_elt) Vec3.t -> t -> t

val position : t -> (float, Bigarray.float32_elt) Vec3.t

val set_rotation : float -> t -> t

val rotation : t -> float

val projection_matrix : t -> (float, Bigarray.float32_elt) Mat4x4.t

val view_matrix : t -> (float, Bigarray.float32_elt) Mat4x4.t

val view_projection_matrix : t -> (float, Bigarray.float32_elt) Mat4x4.t
