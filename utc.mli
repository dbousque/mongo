

type t [@@deriving yojson]

val null : t

val is_null : t -> bool

val get_utc : t -> int64