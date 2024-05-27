type t = {
  name : string;
  typ : DataType.t;
  size : int;
  offset : int;
  normalized : bool;
}

let create ?(offset = 0) ?(normalized = false) name typ size =
  { name; typ; size; offset; normalized }

