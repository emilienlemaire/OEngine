type ('a, 'b) t

val empty : ('a, 'b) Base.elt -> int -> ('a, 'b) t
val zeros : ('a, 'b) Base.elt -> int -> ('a, 'b) t
val ones : ('a, 'b) Base.elt -> int -> ('a, 'b) t
val shape : ('a, 'b) t -> int
val init : ('a, 'b) Base.elt -> int -> (int -> 'a) -> ('a, 'b) t
val scalar_mul : 'a -> ('a, 'b) t -> ('a, 'b) t

type ('a, 'b) vec_op = ('a, 'b) t -> ('a, 'b) t -> ('a, 'b) t

val add : ('a, 'b) vec_op
val sub : ('a, 'b) vec_op
val mul : ('a, 'b) vec_op
val div : ('a, 'b) vec_op
val set : ('a, 'b) t -> int -> 'a -> ('a, 'b) t
val get : ('a, 'b) t -> int -> 'a

module Syntax : sig
  val ( + ) : ('a, 'b) vec_op
  val ( - ) : ('a, 'b) vec_op
  val ( * ) : ('a, 'b) vec_op
  val ( / ) : ('a, 'b) vec_op
  val ( !* ) : 'a -> ('a, 'b) t -> ('a, 'b) t
  val ( .![] ) : ('a, 'b) t -> int -> 'a
  val ( .![]<- ) : ('a, 'b) t -> int -> 'a -> unit
end
