

module type COLLECDESCR =
sig
	val name : string
	type t
	val validate : t -> bool
	val of_yojson : Yojson.Safe.json -> (t, string) Result.result
	val to_yojson : t -> Yojson.Safe.json
end

module type COLLECTION =
sig
	val name : string
	type t
	val validate : t -> bool
	val ok : bool
	val find_one : string -> t option
	val to_yojson : t -> Yojson.Safe.json
	val to_string : t -> string
end

module type MAKECOLLECTION =
	functor (Collec : COLLECDESCR) ->
		COLLECTION with type t = Collec.t

module Make : MAKECOLLECTION =
	functor (Collec : COLLECDESCR) ->
	struct
		let name = Collec.name
		type t = Collec.t
		let validate = Collec.validate
		let ok = true
		let find_one id =
			let json = Yojson.Safe.from_string "{\"name\": \"hello name\", \"age\": 42, \"followers_count\": [[\"One\"], [\"Two\"]]}" in
			match Collec.of_yojson json with
			| Result.Error str -> print_endline str ; None
			| Result.Ok elt -> Some elt
		let to_yojson =
			Collec.to_yojson
		let to_string elt =
			elt |> to_yojson |> Yojson.Safe.to_string
	end