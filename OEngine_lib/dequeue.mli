type 'a t

val nil : 'a t
val make : 'a -> 'a t
val length : 'a t -> int
val append : 'a -> 'a t -> 'a t
val prepend : 'a -> 'a t -> 'a t
val pop : 'a t -> 'a t
val pop_back : 'a t -> 'a t
val remove : 'a -> 'a t -> 'a t
val nth : 'a t -> int -> 'a
val nth_opt : 'a t -> int -> 'a option
val append_dequeue : 'a t -> 'a t -> 'a t
val iter : ('a -> unit) -> 'a t -> unit
val iter_while_false : ('a -> bool) -> 'a t -> unit
val iter_back : ('a -> unit) -> 'a t -> unit
val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
val fold_left_back : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a
val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
val fold_right_back : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
