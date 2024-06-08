type t

val create : unit -> t

val bind : t -> t Core.Error.t

val unbind : t -> t Core.Error.t

val add_vertex_buffer : Buffer.VertexBuffer.t -> t -> t Core.Error.t

val set_index_buffer : Buffer.IndexBuffer.t -> t -> t Core.Error.t

val index_count : t -> int Core.Error.t
