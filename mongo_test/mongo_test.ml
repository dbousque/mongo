

let () =
	let users = Mongo.create "127.0.0.1" 27017 "mongo_ocaml_test" "users" in
	let user = Bson.empty in
	let user = Bson.add_element "key_test" (Bson.create_string "hello_value") user in
	(*Mongo.insert users [user] ; *)
	let search = Bson.empty in
	let search = Bson.add_element "_id" (Bson.create_objectId "589ce6256e627680849b3389") search in
	let reply = Mongo.find_q_one users search in
	print_endline (MongoReply.to_string reply) ;
	let docs = MongoReply.get_document_list reply in
	( match docs with
		| doc::rest -> print_endline (Bson.get_objectId (Bson.get_element "_id" doc))
	) ;
	print_endline "ok"