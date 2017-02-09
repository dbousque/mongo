

type t = string option

let null = None

let of_string str =
	Some str

let to_string = function
	| None -> "null"
	| Some str -> str