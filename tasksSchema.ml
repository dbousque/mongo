

let collection = "tasks"

type t = {
	_id:				ObjectId.t ;
	name:				string ;
	duration:			int ;
	user:				ObjectId.t ;
	age:				int
}
[@@deriving yojson]

let validate doc = false