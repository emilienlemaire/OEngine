type t =
  { name : string;
    typ : DataType.t;
    size : int;
    offset : int;
    normalized : bool
  }

val create : ?offset:int -> ?normalized:bool -> string -> DataType.t -> int -> t
