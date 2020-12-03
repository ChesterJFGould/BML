(* This helps us as we don't have to deal with comments in parsing code and we
 * can get the final location if we get an EOF *)

type t = {
	stream : Tokens.t Stream.t;
	mutable location : Location.t;
	comments : ((string * Location.t) list) ref;
}

let of_channel (ic : in_channel) : t =
	let comments = ref ([] : (string * Location.t) list)
	in let rec next_token (_ : int) : Tokens.t option =
		try
			match Tokens.of_string (input_line ic) with
			| Some (Comment (s, l)) ->
				comments := (s, l)::(!comments);
				next_token 0
			| Some t -> Some t
			| None -> next_token 0
		with End_of_file -> None
	in {
		stream = Stream.from next_token;
		location = Location.of_file "";
		comments = comments;
	}

let next (tq : t) : Tokens.t option =
	try
		let current = Stream.next tq.stream
		in
		tq.location <- Tokens.location current;
		Some current
	with Stream.Failure -> None

let junk (tq : t) : unit =
	try
		let current = Stream.next tq.stream
		in
		tq.location <- Tokens.location current;
		()
	with Stream.Failure -> ()

let rec njunk (n : int) (tq : t) : unit =
	match n with
	| 0 -> ()
	| n -> junk tq; njunk (n - 1) tq

let peek (tq : t) : Tokens.t option = Stream.peek tq.stream

let npeek (n : int) (tq : t) : Tokens.t list = Stream.npeek n tq.stream
