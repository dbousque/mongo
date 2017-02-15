

let () =
	let collec = Mongo.create "localhost" 27017 "test_ocaml_db" "users" in
	let user = Bson.empty in
	let user = Bson.add_element "hi key" (Bson.create_string "hello value") user in
	Mongo.insert collec [user]