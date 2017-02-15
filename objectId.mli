

type t [@@deriving yojson]

val null : t

val is_null : t -> bool

val get_id : t -> string

val of_string : string -> t

val to_string : t -> string
