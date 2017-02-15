

let collection = "users"

type follow = One | Two [@@deriving yojson]

type t = {
	_id:				ObjectId.t ;
	name:				string ;
	age:				int ;
	followers_count:	follow list
}
[@@deriving yojson]

let validate doc = true