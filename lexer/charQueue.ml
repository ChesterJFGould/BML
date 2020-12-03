type t = {
	stream : char Stream.t;
	location : Location.t;
}

let of_channel (ic : in_channel) (file : string) : t = {
	stream = Stream.of_channel ic;
	location = Location.of_file file;
}

let next (cq : t) : char option =
	try
		let current = Stream.next cq.stream
		in if current = '\n' then begin
			cq.location.column <- 1;
			cq.location.line <- cq.location.line + 1;
		end else
			cq.location.column <- cq.location.column + 1;
		Some current
	with Stream.Failure -> None

let junk (cq : t) : unit = 
	try
		let current = Stream.next cq.stream
		in if current = '\n' then begin
			cq.location.column <- 1;
			cq.location.line <- cq.location.line + 1;
		end else
			cq.location.column <- cq.location.column + 1;
		()
	with Stream.Failure -> ()

let rec njunk (n : int) (cq : t) : unit =
	match n with
	| 0 -> ()
	| n -> junk cq; njunk (n - 1) cq

let peek (cq : t) : char option = Stream.peek cq.stream

let npeek (n : int) (cq : t) : char list = Stream.npeek n cq.stream
