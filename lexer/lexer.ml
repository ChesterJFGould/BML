(* Charsets that are used by mutliple functions *)
let whitespace_charset = " \n\t\r"
let identifier_charset = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789+-*/=<>_?!"

(* Matches any sequence of characters delimited by (* *) *)
let comment_tokenizer (cq : CharQueue.t) : (Tokens.t option, string * Location.t) result =
	let location = Location.copy cq.location
	in let rec parse_comment (comment : string) : (Tokens.t option, string * Location.t) result =
		match CharQueue.npeek 2 cq with
		| ['*'; ')'] -> CharQueue.njunk 2 cq; Ok (Some (Comment (comment, location)))
		| c::_ -> CharQueue.junk cq; parse_comment (comment ^ (String.make 1 c))
		| [] -> Ok (Some (Comment (comment, location)))
	in match CharQueue.npeek 2 cq with
	| ['('; '*'] -> CharQueue.njunk 2 cq; parse_comment ""
	| _ -> Ok None

(* Matches any single character symbols that could become a token. *)
let simple_symbol_tokenizer (cq : CharQueue.t) : (Tokens.t option, string * Location.t) result =
	let location = Location.copy cq.location
	in let (token : Tokens.t option)  =
		match CharQueue.peek cq with
		| Some '(' -> Some (LParen location)
		| Some ')' -> Some (RParen location)
		| Some '{' -> Some (LBrace location)
		| Some '}' -> Some (RBrace location)
		| Some '[' -> Some (LBracket location)
		| Some ']' -> Some (RBracket location)
		| Some '+' -> Some (Add location)
		| Some '-' -> Some (Subtract location)
		| Some '*' -> Some (Multiply location)
		| Some '/' -> Some (Divide location)
		| Some '=' -> Some (Equals location)
		| Some '<' -> Some (LessThan location)
		| Some '>' -> Some (GreaterThan location)
		| Some ';' -> Some (Semicolon location)
		| Some '|' -> Some (Bar location)
		| Some '!' -> Some (Not location)
		| _ -> None
	in
	if Option.is_some token then CharQueue.junk cq;
	Ok token

(* Matches any sequence of symbols that could become a token. *)
let long_symbol_tokenizer (cq : CharQueue.t) : (Tokens.t option, string * Location.t) result =
	let location = Location.copy cq.location
	in match CharQueue.npeek 2 cq with
	| ['-'; '>'] -> CharQueue.njunk 2 cq; Ok (Some (Arrow location))
	| ['>'; '='] -> CharQueue.njunk 2 cq; Ok (Some (GreaterThanEqual location))
	| ['<'; '='] -> CharQueue.njunk 2 cq; Ok (Some (LessThanEqual location))
	| _ -> Ok None

(* Matches any sequence of characters delimited by " but parses \" into a ". *)
let string_tokenizer (cq : CharQueue.t) : (Tokens.t option, string * Location.t) result =
	let location = Location.copy cq.location
	in let rec parse_string (s : string) : (Tokens.t option, string * Location.t) result =
		match CharQueue.npeek 2 cq with
		| ['\\'; '"'] -> CharQueue.njunk 2 cq; parse_string (s ^ "\"")
		| '"'::_ -> CharQueue.junk cq; Ok (Some (String (s, location)))
		| c::_ -> CharQueue.junk cq; parse_string (s ^ (String.make 1 c))
		| [] ->
			Error ("Unexpected end of file while parsing string, expected closing \"",
				Location.copy cq.location)
	in match CharQueue.peek cq with
	| Some '"' -> CharQueue.junk cq; parse_string ""
	| _ -> Ok None

(* Matches an int of the form 314 or 159e265, or a float of the form 3.58 or 97.93e23 *)
let number_tokenizer (cq : CharQueue.t) : (Tokens.t option, string * Location.t) result =
	let number_charset = "0123456789"
	in let location = Location.copy cq.location
	(* Just adds TokenQueue.next to the nums string until it isn't a digit *)
	in let rec parse_digits (nums : string) : string =
		match CharQueue.peek cq with
		| Some n when String.contains number_charset n ->
			CharQueue.junk cq;
			parse_digits (nums ^ (String.make 1 n))
		| _ -> nums
	in let parse_float (integer_part : string) : Tokens.t =
		let fractional_part = parse_digits ""
		in match CharQueue.peek cq with
		| Some 'e' ->
			CharQueue.junk cq;
			let exponent = float_of_string ("0"^(parse_digits ""))
			in let base = float_of_string (integer_part ^ "." ^ fractional_part)
			in Float (base ** exponent, location)
		| _ -> Float (float_of_string (integer_part ^ "." ^ fractional_part), location)
	(* Parses an int unless it encounters a ".". If it does it calls parse_float *)
	in let parse_int (first_digit : char) : Tokens.t =
		let base = parse_digits (String.make 1 first_digit)
		in match CharQueue.peek cq with
		| Some '.' -> CharQueue.junk cq; parse_float base
		| Some 'e' ->
			CharQueue.junk cq;
			let exponent = float_of_string ("0" ^ (parse_digits ""))
			in let base = float_of_string base
			in Int (int_of_float (base ** exponent), location)
		| _ -> Int (int_of_string base, location)
	in match CharQueue.peek cq with
	| Some n when String.contains number_charset n -> =
		CharQueue.junk cq;
		Ok (Some (parse_int n))
	| _ -> Ok None

(* Matches a sequence of characters that could be turned into a keyword and that
 * are not followed by another character in the identifier charset. *)
let keyword_tokenizer (cq : CharQueue.t) : (Tokens.t option, string * Location.t) result =
	let location = Location.copy cq.location
	in match CharQueue.npeek 5 cq with
	| ['l'; 'e'; 't'; e; _] when not (String.contains identifier_charset e) ->
		CharQueue.njunk 3 cq; Ok (Some (Let location))
	| ['i'; 'n'; e; _; _] when not (String.contains identifier_charset e) ->
		CharQueue.njunk 2 cq; Ok (Some (In location))
	| ['c'; 'o'; 'n'; 'd'; e] when not (String.contains identifier_charset e) ->
		CharQueue.njunk 4 cq; Ok (Some (Cond location))
	| ['o'; 'f'; e; _; _] when not (String.contains identifier_charset e) ->
		CharQueue.njunk 2 cq; Ok (Some (Of location))
	| ['i'; 'f'; e; _; _] when not (String.contains identifier_charset e) ->
		CharQueue.njunk 2 cq; Ok (Some (If location))
	| ['f'; 'u'; 'n'; e; _] when not (String.contains identifier_charset e) ->
		CharQueue.njunk 3 cq; Ok (Some (Fun location))
	| ['t'; 'y'; 'p'; 'e'; e] when not (String.contains identifier_charset e) ->
		CharQueue.njunk 4 cq; Ok (Some (Type location))
	| ['a'; 'n'; 'd'; e; _] when not (String.contains identifier_charset e) ->
		CharQueue.njunk 3 cq; Ok (Some (And location))
	| ['o'; 'r'; e; _; _] when not (String.contains identifier_charset e) ->
		CharQueue.njunk 2 cq; Ok (Some (Or location))
	| ['t'; 'h'; 'e'; 'n'; e] when not (String.contains identifier_charset e) ->
		CharQueue.njunk 4 cq; Ok (Some (Then location))
	| ['e'; 'l'; 's'; 'e'; e] when not (String.contains identifier_charset e) ->
		CharQueue.njunk 4 cq; Ok (Some (Else location))
	| _ -> Ok None

(* Matches a sequence of characters in the identifier charset *)
let identifier_tokenizer (cq : CharQueue.t) : (Tokens.t option, string * Location.t) result =
	let location = Location.copy cq.location
	in let rec parse_identifier (atom : string) : (Tokens.t option, string * Location.t) result =
		match CharQueue.peek cq with
		| Some w when String.contains whitespace_charset w -> Ok (Some (Identifier (atom, location)))
		| Some c when String.contains identifier_charset c ->
			CharQueue.junk cq; parse_identifier (atom ^ (String.make 1 c))
		| _ -> Ok (Some (Identifier (atom, location)))
	in match CharQueue.peek cq with
	| Some c when String.contains identifier_charset c -> CharQueue.junk cq; parse_identifier (String.make 1 c)
	| _ -> Ok None

(* Tries to tokenize and print the given CharQueue until EOF or an error *)
let rec lex (cq : CharQueue.t) : unit =
	(* A tokenizer function takes in a CharQueue and returns Ok Some if it
	 * matched, Ok None if it didn't match, or Error if there was an error
	 * during matching (for example a string having no closing quote). *)
	let tokenizers = [|
		comment_tokenizer;
		long_symbol_tokenizer;
		simple_symbol_tokenizer;
		string_tokenizer;
		number_tokenizer;
		keyword_tokenizer;
		identifier_tokenizer;
	|]
	in let rec try_tokenize (i : int) : ((Tokens.t, string * Location.t) result) option =
		if i < Array.length tokenizers then
			match tokenizers.(i) cq with
			| Ok (Some t) -> Some (Ok t)
			| Ok None -> try_tokenize (i + 1)
			| Error e -> Some (Error e)
		else None
	in match try_tokenize 0 with
	| Some (Ok t) -> print_endline (Tokens.to_string t); lex cq
	| Some (Error (e, l)) -> Printf.eprintf "Error at %s : %s" (Location.to_string l) e; exit 1
	| None ->
		begin match CharQueue.peek cq with
		| Some w when String.contains whitespace_charset w -> CharQueue.junk cq; lex cq
		| Some c ->
			Printf.eprintf "Error at %s: Unexpected character %C\n"
				(Location.to_string cq.location) c;
			exit 1
		| None -> ()
		end

(* Tokenizes all files passed as arguments or stdin if none are. *)
let () : unit =
	if Array.length Sys.argv > 1 then
		for i = 1 to Array.length Sys.argv - 1 do
			lex (CharQueue.of_channel (open_in Sys.argv.(i)) Sys.argv.(i))
		done
	else
		lex (CharQueue.of_channel stdin "stdin")
