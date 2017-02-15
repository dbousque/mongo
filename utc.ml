

type t = int64 option [@@deriving yojson]

exception Null_Utc

let null = None

let is_null = function
	| None -> true
	| Some _ -> false

let get_utc = function
	| None -> raise Null_Utc
	| Some utc -> utc