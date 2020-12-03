type t = {
	mutable line : int;
	mutable column : int;
	mutable file : string;
}

let of_file (file : string) : t = {
	line = 1;
	column = 1;
	file = file;
}

let copy ({line; column; file} : t) : t = {line; column; file}

let to_string ({line; column; file}: t) : string =
	Printf.sprintf "%d %d %s" line column file

let of_string_list (sl : string list) : t option =
	try
		match sl with
		| [line; column; file] -> Some {
				line =int_of_string line;
				column = int_of_string column;
				file;
			}
		| _ -> None
	with Failure _ -> None
