

type mongot = [
	| `Null
	| `Bool of bool
	| `Int of int
	| `Float of float
	| `String of string
	| `Assoc of (string * mongot) list
	| `List of mongot list
	| `Tuple of mongot list
	| `Variant of (string * mongot option)
	| `ObjectId of ObjectId.t
	| `Utc of Utc.t
]

exception Find_one_failed

module type COLLECDESCR =
sig
	type t
	val collection : string
	val validate : t -> bool
	val of_yojson : Yojson.Safe.json -> (t, string) Result.result
	val to_yojson : t -> Yojson.Safe.json
end

module type COLLECTION =
sig
	type t
	val collection : string
	val validate : t -> bool
	val find : (string * mongot) list -> t list
	val find_one : (string * mongot) list -> t option
	val insert : t -> t
	val to_yojson : t -> Yojson.Safe.json
	val to_string : t -> string
end

module type MAKECOLLECTION =
	functor (Db : Database.DB) (Collec : COLLECDESCR) ->
		COLLECTION with type t = Collec.t

module Make : MAKECOLLECTION =
	functor (Db : Database.DB) (Collec : COLLECDESCR) ->
	struct
		type t = Collec.t
		let collection = Collec.collection
		let validate = Collec.validate
		let collection_connection = Mongo.create Db.host Db.port Db.db collection

		let rec mongot_to_bson = function
			| `Null -> Bson.create_null ()
			| `Bool b -> Bson.create_boolean b
			| `Int i -> Bson.create_int32 (Int32.of_int i)
			| `Float f -> Bson.create_double f
			| `String s -> Bson.create_string s
			| `Assoc doc ->
			| `List vals -> Bson.create_list (List.map mongot_to_bson vals)
			| `Tuple vals -> Bson.create_list (List.map mongot_to_bson vals)
			| `Variant (variant_name, val) -> ( match val with
				| None -> Bson.create_list [Bson.create_string variant_name]
				| Some s -> )
			| `ObjectId id -> ( match id with
				| None -> Bson.create_null ()
				| Some id -> Bson.create_objectid id )
			| `Utc t -> ( match t with
				| None -> Bson.create_null ()
				| Some t -> Bson.create_utc t )

		let query_to_bson query =
			let rec _query_to_bson doc = function
				| [] -> doc
				| (key, val)::rest -> let new_doc = Bson.add_element key (mongot_to_bson val) doc in
										_query_to_bson new_doc rest
			_query_to_bson Bson.empty query

		let find query =
			let query = yojson_to_bson query in
			Mongo.find_q collection_connection 

		let find_one query =
			let json = Yojson.Safe.from_string "{\"_id\": null, \"name\": \"hello name\", \"age\": 42, \"followers_count\": [[\"One\"], [\"Two\"]]}" in
			match Collec.of_yojson json with
			| Result.Error str -> print_endline str ; None
			| Result.Ok elt -> Some elt

		let ffind_one query =
			let json = Yojson.Safe.from_string "{\"_id\": null, \"name\": \"hello name\", \"age\": 42, \"followers_count\": [[\"One\"], [\"Two\"]]}" in
			match Collec.of_yojson json with
			| Result.Error str -> print_endline str ; raise (Find_one_failed)
			| Result.Ok elt -> elt

		let insert doc =
			(* ... *)
			let id = ObjectId.of_string "lolz" in
			doc
		let to_yojson =
			Collec.to_yojson
		let to_string elt =
			elt |> to_yojson |> Yojson.Safe.to_string
	end