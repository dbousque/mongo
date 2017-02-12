

module StdLocal =
struct
	let host = "127.0.0.1"
	let port = 27017
	let db = "std_local_ocaml"
end

module type CONNECTION =
sig
	val host : string
	val port : int
	val db : string
end

module type DB =
sig
	val host : string
	val port : int
	val db : string
	val ok : bool
end

module type MAKEDB =
	functor (Connection : CONNECTION) ->
		DB

module Make : MAKEDB =
	functor (Connection : CONNECTION) ->
	struct
		let host = Connection.host
		let port = Connection.port
		let db = Connection.db
		let ok = true
	end