

exception Find_one_failed

(*let rec bson_to_raw_bson = function
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
	_assoc_to_bson Bson.empty query*)

let adjust_incoming_id = function
	| `Assoc l -> (
			try
				let _id = Yojson.Safe.Util.member "_id" l in
				let _handle_pair = function
					| ("_id", value) -> (
						match value with
						| `Null -> ("_id", ObjectId.null)
						| `ObjectId id -> ("_id", ObjectId.of_string id)
						| _ -> failwith "unexpected _id"
					  )
					| e -> e
				in
				`Assoc (List.map (fun pair -> _handle_pair pair) l)
			with Yojson.Safe.Util.Type_error (msg, _) -> failwith "Document returned by MongoDB didn't have an _id field"
		)
	| _ -> failwith "MongoDB apparently didn't return a document"

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
	val find : (string * Bson.element) list -> (t option) list
	val ffind : (string * Bson.element) list -> t list
	val find_one : (string * Bson.element) list -> t option
	val ffind_one : (string * Bson.element) list -> t
	val insert : t -> t
	val to_yojson : t -> Yojson.Safe.json
	val to_string : t -> string
	val to_pstring : t -> string
	val pprint : t -> unit
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

		let raw_bson_to_yojson bson =
			`Assoc [("lolz", `Int 4)]

		let insert doc =
			let bson = doc |> Collec.to_yojson |> Bson.of_yojson in
			Mongo.insert collection_connection [Bson.get_doc_element bson] ;
			doc

		let find query =
			let _of_yojson_option doc =
				match Collec.of_yojson doc with
				| Result.Error err -> print_endline ("error decoding : " ^ err) ; None
				| Result.Ok elt -> Some elt
			in
			let reply = Mongo.find_q_one collection_connection query in
			let docs = MongoReply.get_document_list reply in
			List.map (fun x -> x |> raw_bson_to_yojson |> adjust_incoming_id |> _of_yojson_option) docs

		let ffind query =
			let _of_yojson_fail doc =
				match Collec.of_yojson doc with
				| Result.Error err -> failwith ("error decoding : " ^ err)
				| Result.Ok elt -> elt
			in
			let reply = Mongo.find_q_one collection_connection query in
			let docs = MongoReply.get_document_list reply in
			List.map (fun x -> x |> raw_bson_to_yojson |> adjust_incoming_id |> _of_yojson_fail) docs

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

		let pprint elt =
			elt |> to_pstring |> print_endline
	end