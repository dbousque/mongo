

open Yojson.Safe

type t = string option [@@deriving yojson]

exception Null_ObjectId

let null = None

let is_null = function
	| None -> true
	| Some _ -> false

let get_id = function
	| None -> raise Null_ObjectId
	| Some id -> id

let of_string str =
	Some str

let to_string = function
	| None -> "null"
	| Some str -> str

let to_bson = function
	| None -> Bson.Null
	| Some id -> Bson.ObjectId id

let of_yojson elt =
	match elt with
	| `Intlit "" -> Result.Ok None
	| `Intlit id -> Result.Ok (Some id)
	| _ -> Result.Error "objectids are supposed to be encoded as `Intlit in yojson"

let to_yojson oid =
	match oid with
	| None -> `Intlit ""
	| Some id -> `Intlit id