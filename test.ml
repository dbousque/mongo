

open Mybson

module Connection = struct
	include Database.StdLocal
	let db = "mongo_test_ocaml"
end

module Db = Database.Make (Database.StdLocal)

module Users = Collection.Make (Db) (UsersSchema)

module Tasks = Collection.Make (Db) (TasksSchema)


(*module UsersFinder = Users.makeFinder (FinderSchema)*)

let print_bool = function
	| true -> print_endline "true"
	| false -> print_endline "false"

let () =
	(*let user = Users.find_one "54759eb3c090d83494e2d804" in
	( match user with
	| None -> print_endline "error"
	| Some elt -> elt |> Users.to_string |> print_endline ) ; *)
	let my_user = UsersSchema.{
		_id = ObjectId.null ;
		name = "hello" ;
		age = 16 ;
		followers_count = [One ; Two ; One] 
	} in
	(*
	print_endline (Users.to_string my_user) ;
	let my_task = Tasks.find "54759eb3c090d83494e2d804" in
	print_endline (Tasks.to_string my_task) ;
	let my_task2 = TasksSchema.{
		_id = ObjectId.null ;
		name = "nice task" ;
		duration = 12 ;
		user = ObjectId.null ;
		age = my_user.UsersSchema.age
	} in
	print_endline (Tasks.to_string my_task2) ;
	print_bool (Users.validate my_user) ;
	print_bool (Tasks.validate my_task2) ; *)
	(*let query = FinderSchema.{
		name = 
	} in
	UsersFinder.run query *)
	let docs = Users.find [(
			"qty", Assoc [("$gt", Int 4)]
		) ;
		(
			"user", ObjectId my_user.UsersSchema._id
		) ;
		(
			"followers_count", List [Yojson (UsersSchema.follow_to_yojson UsersSchema.One)]
		)
	] in
	let my_doc = Users.find_one [(
		"name", String "dodo"
	)] in
	(*
	{
		"users": {"$gt": 4}
	}
	[(
		"users", `Assoc [("$gt", `Int 4)]
	)]
	*)
	List.map (fun doc -> doc |> Users.to_pstring |> print_endline) docs ;
	print_endline "ok"


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