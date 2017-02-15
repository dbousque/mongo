

type mybson =
	| Null
	| Bool of bool
	| Int of int
	| Int64 of int64
	| Float of float
	| String of string
	| Assoc of (string * mybson) list
	| List of mybson list
	| Tuple of mybson list
	| Yojson of Yojson.Safe.json
	| ObjectId of ObjectId.t
	| Utc of Utc.t