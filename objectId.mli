

type t [@@deriving yojson]

val null : t

val of_string : string -> t

val to_string : t -> string
