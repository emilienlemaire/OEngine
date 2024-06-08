type ('a, 'b) t

val empty : ('a, 'b) Base.elt -> ('a, 'b) t

val zeros : ('a, 'b) Base.elt -> ('a, 'b) t

val ones : ('a, 'b) Base.elt -> ('a, 'b) t

val shape : ('a, 'b) t -> int

val init : ('a, 'b) Base.elt -> (int -> 'a) -> ('a, 'b) t

val of_scalars : ('a, 'b) Base.elt -> 'a -> 'a -> 'a -> ('a, 'b) t

val scalar_mul : 'a -> ('a, 'b) t -> ('a, 'b) t

type ('a, 'b) vec_op = ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t

val add : ('a, 'b) vec_op

val sub : ('a, 'b) vec_op

val mul : ('a, 'b) vec_op

val div : ('a, 'b) vec_op

val dot : ('a, 'b) t -> ('a, 'b) t -> 'a

val set : ('a, 'b) t -> int -> 'a -> ('a, 'b) t

val get : ('a, 'b) t -> int -> 'a

val normalize : (float, 'b) t -> (float, 'b) t

val x : ('a, 'b) t -> 'a

val y : ('a, 'b) t -> 'a

val z : ('a, 'b) t -> 'a

val r : ('a, 'b) t -> 'a

val g : ('a, 'b) t -> 'a

val b : ('a, 'b) t -> 'a

val s : ('a, 'b) t -> 'a

val t : ('a, 'b) t -> 'a

val p : ('a, 'b) t -> 'a

val as_genarray : ('a, 'b) t -> ('a, 'b, Bigarray.c_layout) Bigarray.Genarray.t

module Syntax : sig
  val ( ^+^ ) : ('a, 'b) vec_op

  val ( ^-^ ) : ('a, 'b) vec_op

  val ( ^*^ ) : ('a, 'b) vec_op

  val ( ^/^ ) : ('a, 'b) vec_op

  val ( *^ ) : 'a -> ('a, 'b) t -> ('a, 'b) t

  val ( /^ ) : 'a -> ('a, 'b) t -> ('a, 'b) t

  val ( ^/ ) : ('a, 'b) t -> 'a -> ('a, 'b) t

  val ( .^[] ) : ('a, 'b) t -> int -> 'a

  val ( .^[]<- ) : ('a, 'b) t -> int -> 'a -> unit
end
