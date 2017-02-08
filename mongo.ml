

type follow = One | Two [@@deriving yojson]

module UsersCollection =
struct
	let name = "users"
	type t = {
		name:				string ;
		age:				int ;
		followers_count:	follow list
	}
	[@@deriving yojson]
	let validate doc = true
end

module Users = Collection.Make (UsersCollection)

let () =
	let user = Users.find_one "54759eb3c090d83494e2d804" in
	match user with
	| None -> print_endline "error"
	| Some elt -> elt |> Users.to_string |> print_endline


(*open Yojson.Basic.Util

let extract_titles json =
	[json]
	|> filter_member "pages"
	|> flatten
	|> filter_member "title"
	|> filter_string

let () =
	let json = Yojson.Safe.from_string "{\"hello\": 12}" in
	print_endline (Yojson.Safe.to_string json) ;
	print_endline "ok"*)
	(*List.iter print_endline (extract_titles json) *)