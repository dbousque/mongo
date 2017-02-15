

open Mybson

exception Find_one_failed

let rec yojson_to_bson = function
	| `Null -> Bson.create_null ()
	| `Bool b -> Bson.create_boolean b
	| `Int i -> Bson.create_int32 (Int32.of_int i)
	| `Float f -> Bson.create_double f
	| `String s -> Bson.create_string s
	| `Intlit s -> Bson.create_string s
	| `List l -> Bson.create_list (List.map yojson_to_bson l)
	| `Tuple l -> Bson.create_list (List.map yojson_to_bson l)
	| `Assoc l -> Bson.create_doc_element (yojson_assoc_to_bson l)
	| `Variant (k, None) -> Bson.create_string k
	| `Variant (k, Some v) -> Bson.create_list [Bson.create_string k ; yojson_to_bson v]

and yojson_assoc_to_bson yojson =
	let rec _yojson_assoc_to_bson doc = function
		| [] -> doc
		| (key, value)::rest -> ( let new_doc = Bson.add_element key (yojson_to_bson value) doc in
								_yojson_assoc_to_bson new_doc rest )
	in
	_yojson_assoc_to_bson Bson.empty yojson

let rec bson_to_raw_bson = function
	| Null -> Bson.create_null ()
	| Bool b -> Bson.create_boolean b
	| Int i -> Bson.create_int32 (Int32.of_int i)
	| Int64 i -> Bson.create_int64 i
	| Float f -> Bson.create_double f
	| String s -> Bson.create_string s
	| Assoc doc -> Bson.create_doc_element (assoc_to_raw_bson doc)
	| List vals -> Bson.create_list (List.map bson_to_raw_bson vals)
	| Tuple vals -> Bson.create_list (List.map bson_to_raw_bson vals)
	| Yojson json -> yojson_to_bson json
	| ObjectId id -> ( match ObjectId.is_null id with
		| true -> Bson.create_null ()
		| false -> Bson.create_objectId (ObjectId.get_id id) )
	| Utc t -> ( match Utc.is_null t with
		| true -> Bson.create_null ()
		| false -> Bson.create_utc (Utc.get_utc t) )

and assoc_to_raw_bson query =
	let rec _assoc_to_bson doc = function
		| [] -> doc
		| (key, value)::rest -> let new_doc = Bson.add_element key (bson_to_raw_bson value) doc in
								_assoc_to_bson new_doc rest
	in
	_assoc_to_bson Bson.empty query

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
	val find : (string * Mybson.mybson) list -> t list
	val find_one : (string * Mybson.mybson) list -> t option
	val insert : t -> t
	val to_yojson : t -> Yojson.Safe.json
	val to_string : t -> string
	val to_pstring : t -> string
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

		let raw_bson_to_yojson =

		let insert doc =
			doc

		let find query =
			let raw_bson = assoc_to_raw_bson query in
			let reply = Mongo.find_q_one collection_connection raw_bson in
			let docs = MongoReply.get_document_list reply in
			List.map (fun x -> x |> raw_bson_to_yojson |> of_yojson) docs

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

		let to_yojson =
			Collec.to_yojson

		let to_string elt =
			elt |> to_yojson |> Yojson.Safe.to_string

		let to_pstring elt =
			elt |> to_yojson |> Yojson.Safe.pretty_to_string
	end