exception ParseError of (string * Location.t)

let unexpected_token (token : Tokens.t) (expected : string) : string * Location.t=
	(Printf.sprintf "Unexpected token \"%s\", expected %s"
		(Tokens.literal token) expected, Tokens.location token)

let unexpected_eof (l : Location.t) (expected : string) : string * Location.t =
	(Printf.sprintf "Unexpected end of file, expected %s" expected, l)

let expect_token (t0 : Tokens.t) (tq : TokenQueue.t) (expected : string) : unit =
	match TokenQueue.next tq with
	| Some t1 when Tokens.is_same_kind t0 t1 -> ()
	| Some t -> raise (ParseError (unexpected_token t expected))
	| None -> raise (ParseError (unexpected_eof tq.location expected))

let get_token (t0 : Tokens.t) (tq : TokenQueue.t) (expected : string) : Tokens.t =
	match TokenQueue.next tq with
	| Some t1 when Tokens.is_same_kind t0 t1 -> t1
	| Some t -> raise (ParseError (unexpected_token t expected))
	| None -> raise (ParseError (unexpected_eof tq.location expected))

let get_identifier (tq : TokenQueue.t) : string * Location.t =
	match TokenQueue.next tq with
	| Some (Identifier (i, l)) -> (i, l)
	| Some t -> raise (ParseError (unexpected_token t "an identifier"))
	| None -> raise (ParseError (unexpected_eof tq.location "an identifier"))

let get_any_token (tq : TokenQueue.t) (expected : string) : Tokens.t =
	match TokenQueue.next tq with
	| Some t -> t
	| None -> raise (ParseError (unexpected_eof tq.location expected))

let precedence (token : Tokens.t) : int =
	match token with
	| And _ | Or _ -> 1
	| Equals _ | LessThan _ | GreaterThan _ | LessThanEqual _
	| GreaterThanEqual _ -> 2
	| Add _ | Subtract _ -> 3
	| Multiply _ | Divide _ -> 4
	| _ -> 0

let definition_token (token : Tokens.t) : bool =
	match token with
	| Let _ -> true
	| Type _ -> true
	| _ -> false

let rec parse_identity (tq : TokenQueue.t) : Nodes.expr =
	(* prefix parser for tokens whose node equivalent is literally the same.
	 * e.g. String (string, location) -> String (string, location) *)
	match get_any_token tq "atomic data (This error shouldn't ever happen)" with
	| Int (i, l) -> Int (i, l)
	| Float (f, l) -> Float (f, l)
	| String (s, l) -> String (s, l)
	| Identifier (n, l) -> Identifier (n, l)
	| t -> raise (ParseError (unexpected_token t
		"an identity token (This shouldn't ever happen)"))

and parse_parentheses (tq : TokenQueue.t) : Nodes.expr =
	expect_token (LParen tq.location) tq "an opening parenthesis";
	let expr = parse_expression tq 0
	in expect_token (RParen tq.location) tq "an closing parenthesis";
	expr

and parse_local_definition (tq : TokenQueue.t) : Nodes.expr =
	let def = parse_definition tq
	in let l = Tokens.location (get_token (In tq.location) tq "a local definition")
	in let expr = parse_expression tq 0
	in Local (def, expr, l)

and parse_list (tq : TokenQueue.t) : Nodes.expr =
	let rec parse_elements (elements : Nodes.expr list) : Nodes.expr array =
		let expr = parse_expression tq 0
		in match TokenQueue.npeek 2 tq with
		| [Semicolon _; RBracket _] ->
			TokenQueue.junk tq;
			Array.of_list (List.rev (expr::elements))
		| [Semicolon _; _] ->
			TokenQueue.junk tq;
			parse_elements (expr::elements)
		| _ -> Array.of_list (List.rev (expr::elements))
	in match TokenQueue.npeek 2 tq with
	| [LBracket l; RBracket _] -> TokenQueue.njunk 2 tq; List ([||], l)
	| [LBracket l; _] ->
		TokenQueue.junk tq;
		let elements = parse_elements []
		in expect_token (RBracket tq.location) tq "a closing bracket";
		List (elements, l)
	| t::_ -> raise (ParseError (unexpected_token t "a list"))
	| [] -> raise (ParseError (unexpected_eof tq.location "a list"))

and parse_lambda (tq : TokenQueue.t) : Nodes.expr =
	let rec parse_arguments (args : string list) : string array =
		match TokenQueue.peek tq with
		| Some (Identifier (arg, _)) -> TokenQueue.junk tq; parse_arguments (arg::args)
		| _ -> Array.of_list (List.rev args)
	in let l = Tokens.location (get_token (Fun tq.location) tq "an anonymous function")
	in let args = parse_arguments []
	in expect_token (Arrow tq.location) tq "a function body";
	let body = parse_expression tq 0
	in Lambda(args, body, l)

and parse_cond (tq : TokenQueue.t) : Nodes.expr =
	let parse_case () : Nodes.expr * Nodes.expr =
		let condition = parse_expression tq 0
		in expect_token (Arrow tq.location) tq "a cond result";
		let consequence = parse_expression tq 0
		in (condition, consequence)
	in let rec parse_cases (cases : (Nodes.expr * Nodes.expr) list) : (Nodes.expr * Nodes.expr) array =
		match cases with
		(* we do this so that the first | is optional *)
		| [] ->
			begin match TokenQueue.peek tq with
			| Some (Bar _) ->
				TokenQueue.junk tq;
			| _ -> ()
			end;
			parse_cases ((parse_case ())::cases)
		| _ ->
			begin match TokenQueue.peek tq with
			| Some (Bar _) ->
				TokenQueue.junk tq;
				parse_cases ((parse_case ())::cases)
			| _ -> Array.of_list (List.rev cases)
			end
	in let l = Tokens.location (get_token (Cond tq.location) tq "a cond expression")
	in let cases = parse_cases []
	in Cond (cases, l)

and parse_if (tq : TokenQueue.t) : Nodes.expr =
	let l = Tokens.location (get_token (If tq.location) tq "an if expression")
	in let condition = parse_expression tq 0
	in expect_token (Then tq.location) tq "\"then\"";
	let consequence = parse_expression tq 0
	in expect_token (Else tq.location) tq "\"else\"";
	let alternative = parse_expression tq 0
	in If (condition, consequence, alternative, l)

and parse_else_identifier (tq : TokenQueue.t) : Nodes.expr =
	(* This is how we handle the "else" case in a cond, if an else token is
	 * encountered in a prefix position we just convert it to an identifier.
	 * A better way to do this would to just handle it and convert it explicitly
	 * inside the cond parser
	 *)
	 let l = Tokens.location (get_token (Else tq.location) tq "an else")
	 in Identifier ("else", l)

and parse_prefix_operator (tq : TokenQueue.t) : Nodes.expr =
	let identifier_of_token (token : Tokens.t) : Nodes.expr =
		match token with
		| Add l -> Identifier ("+", l)
		| Subtract l -> Identifier ("-", l)
		| Multiply l -> Identifier ("*", l)
		| Divide l -> Identifier ("/", l)
		| And l -> Identifier ("and", l)
		| Or l -> Identifier ("or", l)
		| Not l -> Identifier ("not", l)
		| Equals l -> Identifier ("=", l)
		| GreaterThan l -> Identifier (">", l)
		| LessThan l -> Identifier ("<", l)
		| GreaterThanEqual l -> Identifier (">=", l)
		| LessThanEqual l -> Identifier ("<=", l)
		| t -> raise (ParseError (unexpected_token t
			"a prefix operator (This error should never happen)"))
	in match TokenQueue.next tq with
	| Some t when Tokens.is_prefix_operator t -> identifier_of_token t
	| Some t -> raise (ParseError (unexpected_token t "an operator"))
	| None -> raise (ParseError (unexpected_eof tq.location "an operator"))

and prefix_parser (token : Tokens.t) : (TokenQueue.t -> Nodes.expr) option =
	match token with
	| Int _ | Float _ | String _ | Identifier _ -> Some parse_identity
	| LParen _ -> Some parse_parentheses
	| Let _ | Type _ -> Some parse_local_definition
	| LBracket _ -> Some parse_list
	| Fun _ -> Some parse_lambda
	| Cond _ -> Some parse_cond
	| If _ -> Some parse_if
	| Else _ -> Some parse_else_identifier
	| t when Tokens.is_prefix_operator t -> Some parse_prefix_operator
	| _ -> None

and parse_infix_operator (tq : TokenQueue.t) (left : Nodes.expr) : Nodes.expr =
	let op_from_token (t : Tokens.t) : Nodes.op =
		match t with
		| Add _ -> Add
		| Subtract _ -> Subtract
		| Multiply _ -> Multiply
		| Divide _ -> Divide
		| And _ -> And
		| Or _ -> Or
		| Equals _ -> Equals
		| GreaterThan _ -> GreaterThan
		| LessThan _ -> LessThan
		| GreaterThanEqual _ -> GreaterThanEqual
		| LessThanEqual _ -> LessThanEqual
		| t -> raise (ParseError (unexpected_token t
			"an infix operator (This error should never happen)"))
	in let t = get_any_token tq "an infix operator"
	in let op = op_from_token t
	in let right = parse_expression tq (precedence t)
	in Operator(op, left, right, Tokens.location t)

and infix_parser (token : Tokens.t)
: (TokenQueue.t -> Nodes.expr -> Nodes.expr) option =
	match token with
	| t when Tokens.is_infix_operator t -> Some parse_infix_operator
	| _ -> None
	
and parse_application (tq : TokenQueue.t) : Nodes.expr =
	(* 
	 * Essentially parse_expression but we pretend there is an application operator
	 * after each argument node.
	 *)
	let parse_prefix () : Nodes.expr =
		match TokenQueue.peek tq with
		| Some t ->
			begin match prefix_parser t with
			| Some parser -> parser tq
			| None -> raise (ParseError (unexpected_token t "an expression"))
			end
		| None -> raise (ParseError (unexpected_eof tq.location "an expression"))
	
	in let rec parse_arguments (args : Nodes.expr list) : Nodes.expr array =
		match TokenQueue.peek tq with
		| Some t when Tokens.is_argument t ->
			begin match prefix_parser t with
			| Some parser -> parse_arguments ((parser tq)::args)
			(* If this triggers then we haven't written/registered a prefix parser for
			 * an argument token.
			 *)
			| None -> raise (ParseError (unexpected_token t "an argument"))
			end
		| _ -> Array.of_list (List.rev args)
	in let func = parse_prefix ()
	in match TokenQueue.peek tq with
	| Some t when Tokens.is_argument t ->
		let args = parse_arguments []
		in Application (func, args, Nodes.location func)
	| _ -> func

and parse_expression (tq : TokenQueue.t) (prec : int) : Nodes.expr =
	let rec parse_infix (left : Nodes.expr) : Nodes.expr =
		match TokenQueue.peek tq with
		| Some t when precedence t > prec ->
			begin match infix_parser t with
			| Some parser -> parse_infix (parser tq left)
			| None -> left
			end
		| _ -> left
	in parse_infix (parse_application tq)

and parse_let_definition (tq : TokenQueue.t) : Nodes.def =
	let rec parse_arguments (args : string list) : string array =
		match TokenQueue.peek tq with
		| Some (Identifier (arg, _)) -> TokenQueue.junk tq; parse_arguments (arg::args)
		| _ -> Array.of_list (List.rev args)	
	in let l = Tokens.location (get_token (Let tq.location) tq "a local definition")
	in let (name, _) = get_identifier tq
	in match parse_arguments [] with
	| [||] -> expect_token (Equals tq.location) tq "an assignment";
		let value = parse_expression tq 0
		in Variable (name, value, l)
	| args -> expect_token (Equals tq.location) tq "an assignment";
		let body = parse_expression tq 0
		in Function (name, args, body, l)

and parse_type_definition (tq : TokenQueue.t) : Nodes.def =
	let rec parse_members (members : string list) : string array =
		match TokenQueue.npeek 2 tq with
		| [Identifier (member, _); Semicolon _] ->
			TokenQueue.njunk 2 tq; parse_members (member::members)
		| [Identifier (member, _); _] ->
			TokenQueue.junk tq; Array.of_list (List.rev (member::members))
		| _ -> Array.of_list (List.rev members)
	in let parse_struct () : string array =
		expect_token (LBrace tq.location) tq "a struct";
		let members = parse_members []
		in expect_token (RBrace tq.location) tq "a closing bracket";
		members
	in let l = Tokens.location (get_token (Type tq.location) tq "a type")
	in let (name, _) = get_identifier tq
	in expect_token (Equals tq.location) tq "an assignment";
	let members = parse_struct ()
	in Type (name, members, l)

and parse_definition (tq : TokenQueue.t) : Nodes.def =
	match TokenQueue.peek tq with
	| Some (Let _) -> parse_let_definition tq
	| Some (Type _) -> parse_type_definition tq
	| Some t -> raise (ParseError (unexpected_token t "a definition"))
	| None -> raise (ParseError (unexpected_eof tq.location "a definition"))

let rec parse_statement (tq : TokenQueue.t) : Nodes.statement option =
	match TokenQueue.peek tq with
	| Some (Let _) | Some (Type _) ->
		let def = parse_definition tq
		in begin match TokenQueue.peek tq with
		| Some (In l) ->
			TokenQueue.junk tq;
			let body = parse_expression tq 0
			in Some (Expression (Local (def, body, l)))
		| _ -> Some (Definition def)
		end
	| Some (Semicolon _) -> TokenQueue.junk tq; parse_statement tq
	| Some _ ->
		let expr = parse_expression tq 0
		in Some (Expression expr)
	| None -> None

let parse_program (tq : TokenQueue.t) : Nodes.program =
	let rec parse_until_eof (prog : Nodes.program) : Nodes.program =
		match parse_statement tq with
		| Some s -> parse_until_eof (s::prog)
		| None -> List.rev prog
	in parse_until_eof []

let () : unit =
	let parse_print_program (tq : TokenQueue.t) : unit =
		try
			let prog = parse_program tq
			in Nodes.serialize_program stdout prog
		with ParseError (e, l) ->
			Printf.eprintf "Error at %s : %s\n" (Location.to_string l) e; exit 1
	in if Array.length Sys.argv > 1 then
		for i = 1 to Array.length Sys.argv - 1 do
			parse_print_program (TokenQueue.of_channel (open_in Sys.argv.(i)))
		done
	else
		parse_print_program (TokenQueue.of_channel stdin)
