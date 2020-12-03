type error = string * Location.t

let unexpected_token (token : Tokens.t) (expected : string) : error =
	(Printf.sprintf "Unexpected token \"%s\", expected %s"
		(Tokens.literal token) expected, Tokens.location token)

let unexpected_eof (l : Location.t) (expected : string) : error =
	(Printf.sprintf "Unexpected end of file, expected %s" expected, l)

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

let rec parse_identity (tq : TokenQueue.t) : (Nodes.expr, error) result =
	(* prefix parser for tokens whose node equivalent is literal the same.
	 * e.g. String (string, location) -> String (string, location) *)
	let expr_of_token (token : Tokens.t) : (Nodes.expr, error) result =
		match token with
		| Int (i, l) -> Ok (Int (i, l))
		| Float (f, l) -> Ok (Float (f, l))
		| String (s, l) -> Ok (String (s, l))
		| Identifier (n, l) -> Ok (Identifier (n, l))
		| t -> Error (unexpected_token t "an identity token (This error means the parser is broken)")
	in match TokenQueue.next tq with
	| Some t -> expr_of_token t
	| None -> Error (unexpected_eof tq.location "atomic data")

and parse_parentheses (tq : TokenQueue.t) : (Nodes.expr, error) result =
	match TokenQueue.next tq with
	| Some (LParen _) ->
		begin match parse_expression tq 0 with
		| Ok expr ->
			begin match TokenQueue.next tq with
			| Some (RParen _) -> Ok expr
			| Some t -> Error (unexpected_token t "a closing parenthesis")
			| None -> Error (unexpected_eof tq.location " a closing parenthesis")
			end
		| Error e -> Error e
		end
	| Some t -> Error (unexpected_token t "an opening parenthesis")
	| None -> Error (unexpected_eof tq.location "an opening parenthesis")

and parse_local_definition (tq : TokenQueue.t) : (Nodes.expr, error) result =
	match parse_definition tq with
	| Ok def ->
		begin match TokenQueue.next tq with
		| Some (In l) -> 
			begin match parse_expression tq 0 with
			| Ok expr -> Ok (Local (def, expr, l))
			| Error e -> Error e
			end
		| Some t -> Error (unexpected_token t "a local definition")
		| None -> Error (unexpected_eof tq.location "a local definition")
		end
	| Error e -> Error e

and parse_list (tq : TokenQueue.t) : (Nodes.expr, error) result =
	let rec parse_elements (elements : Nodes.expr list) : (Nodes.expr array, error) result =
		let expr = parse_expression tq 0
		in match (expr, TokenQueue.npeek 2 tq) with
		| (Ok expr, [Semicolon _; RBracket _]) ->
			TokenQueue.junk tq;
			Ok (Array.of_list (List.rev (expr::elements)))
		| (Ok expr, [Semicolon _; _]) ->
			TokenQueue.junk tq;
			parse_elements (expr::elements)
		| (Ok expr, _) -> Ok (Array.of_list (List.rev (expr::elements)))
		| (Error e, _) -> Error e
	in match TokenQueue.npeek 2 tq with
	| [LBracket l; RBracket _] -> TokenQueue.njunk 2 tq; Ok (List ([||], l))
	| [LBracket l; _] ->
		TokenQueue.junk tq;
		begin match parse_elements [] with
		| Ok elements ->
			begin match TokenQueue.next tq with
			| Some (RBracket _) -> Ok (List (elements, l))
			| Some t -> Error (unexpected_token t "a closing bracket")
			| None -> Error (unexpected_eof tq.location "a closing bracket")
			end
		| Error e -> Error e
		end
	| t::_ -> Error (unexpected_token t "a list")
	| [] -> Error (unexpected_eof tq.location "a list")

and parse_lambda (tq : TokenQueue.t) : (Nodes.expr, error) result =
	let rec parse_arguments (args : string list) : string array =
		match TokenQueue.peek tq with
		| Some (Identifier (arg, _)) -> TokenQueue.junk tq; parse_arguments (arg::args)
		| _ -> Array.of_list (List.rev args)
	in match TokenQueue.next tq with
	| Some (Fun l) ->
		let args = parse_arguments []
		in begin match TokenQueue.next tq with
		| Some (Arrow _) ->
			begin match parse_expression tq 0 with
			| Ok body -> Ok (Lambda (args, body, l))
			| Error e -> Error e
			end
		| Some t -> Error (unexpected_token t "a function body")
		| None -> Error (unexpected_eof tq.location "a function body")
		end
	| Some t -> Error (unexpected_token t "an anonymous function")
	| None -> Error (unexpected_eof tq.location "an anonymous function")

and parse_cond (tq : TokenQueue.t) : (Nodes.expr, error) result =
	let parse_case () : (Nodes.expr * Nodes.expr, error) result =
		match parse_expression tq 0 with
		| Ok cond -> 
			begin match TokenQueue.next tq with
			| Some (Arrow _) ->
				begin match parse_expression tq 0 with
				| Ok result -> Ok (cond, result)
				| Error e -> Error e
				end
			| Some t -> Error (unexpected_token t "a cond result")
			| None -> Error (unexpected_eof tq.location "a cond result")
			end
		| Error e -> Error e
	in let rec parse_cases (cases : (Nodes.expr * Nodes.expr) list) : ((Nodes.expr * Nodes.expr) array, error) result =
		match cases with
		(* we do this so that the first | is optional *)
		| [] ->
			begin match TokenQueue.peek tq with
			| Some (Bar _) ->
				TokenQueue.junk tq;
				begin match parse_case () with
				| Ok case -> parse_cases (case::cases)
				| Error e -> Error e
				end
			| _ -> 
				begin match parse_case () with
				| Ok case -> parse_cases (case::cases)
				| Error e -> Error e
				end
			end
		| _ ->
			begin match TokenQueue.peek tq with
			| Some (Bar _) ->
				TokenQueue.junk tq;
				begin match parse_case () with
				| Ok case -> parse_cases (case::cases)
				| Error e -> Error e
				end
			| _ -> Ok (Array.of_list (List.rev cases))
			end
	in match TokenQueue.next tq with
	| Some (Cond l) ->
		begin match parse_cases [] with
		| Ok cases -> Ok (Cond (cases, l))
		| Error e -> Error e
		end
	| Some t -> Error (unexpected_token t "a cond expression")
	| None -> Error (unexpected_eof tq.location "a cond expression")

and parse_prefix_operator (tq : TokenQueue.t) : (Nodes.expr, error) result =
	let identifier_of_token (token : Tokens.t) : (Nodes.expr, error) result =
		match token with
		| Add l -> Ok (Identifier ("+", l))
		| Subtract l -> Ok (Identifier ("-", l))
		| Multiply l -> Ok (Identifier ("*", l))
		| Divide l -> Ok (Identifier ("/", l))
		| And l -> Ok (Identifier ("and", l))
		| Or l -> Ok (Identifier ("or", l))
		| Not l -> Ok (Identifier ("not", l))
		| Equals l -> Ok (Identifier ("=", l))
		| GreaterThan l -> Ok (Identifier (">", l))
		| LessThan l -> Ok (Identifier ("<", l))
		| GreaterThanEqual l -> Ok (Identifier (">=", l))
		| LessThanEqual l -> Ok (Identifier ("<=", l))
		| t -> Error (unexpected_token t "a prefix binary operator token (This error means the parser is broken)")
	in match TokenQueue.next tq with
	| Some t when Tokens.is_prefix_operator t -> identifier_of_token t
	| Some t -> Error (unexpected_token t "an operator")
	| None -> Error (unexpected_eof tq.location "an operator")

and prefix_parser (token : Tokens.t) : (TokenQueue.t -> ((Nodes.expr, error) result)) option =
	match token with
	| Int _ | Float _ | String _ | Identifier _ -> Some parse_identity
	| LParen _ -> Some parse_parentheses
	| Let _ | Type _ -> Some parse_local_definition
	| LBracket _ -> Some parse_list
	| Fun _ -> Some parse_lambda
	| Cond _ -> Some parse_cond
	| t when Tokens.is_prefix_operator t -> Some parse_prefix_operator
	| _ -> None

and parse_binary_operator (tq : TokenQueue.t) (left : Nodes.expr) : (Nodes.expr, error) result =
	let op_of_token (token : Tokens.t) : (Nodes.op, error) result =
		match token with
		| Add _ -> Ok Add
		| Subtract _ -> Ok Subtract
		| Multiply _ -> Ok Multiply
		| Divide _ -> Ok Divide
		| And _ -> Ok And
		| Or _ -> Ok Or
		| Equals _ -> Ok Equals
		| GreaterThan _ -> Ok GreaterThan
		| LessThan _ -> Ok LessThan
		| GreaterThanEqual _ -> Ok GreaterThanEqual
		| LessThanEqual _ -> Ok LessThanEqual
		| t -> Error (unexpected_token t "a binary operator token (This error means the parser is broken)")
	in match TokenQueue.next tq with
	| Some t when Tokens.is_infix_operator t ->
		begin match (parse_expression tq (precedence t), op_of_token t) with
		| (Ok right, Ok op) -> Ok (Operator (op, left, right, (Tokens.location t)))
		| (Error e, _) -> Error e
		| (_, Error e) -> Error e
		end
	| Some t -> Error (unexpected_token t "an operator")
	| None -> Error (unexpected_eof tq.location "an operator")

and infix_parser (token : Tokens.t)
: (TokenQueue.t -> Nodes.expr -> ((Nodes.expr, error) result)) option =
	match token with
	| t when Tokens.is_infix_operator t -> Some parse_binary_operator
	| _ -> None
	
and parse_application (tq : TokenQueue.t) : (Nodes.expr, error) result =
	(* 
	 * Essentially parse_expression but we pretend there is an application operator
	 * after each argument node.
	 *)
	let parse_func () : (Nodes.expr, error) result =
		match TokenQueue.peek tq with
		| Some t ->
			begin match prefix_parser t with
			| Some parser ->
				begin match parser tq with
				| Ok func -> Ok func
				| Error e -> Error e
				end
			| None -> Error (unexpected_token t "an expression")
			end
		| None -> Error (unexpected_eof tq.location "an expression")
	in let rec parse_arguments (args : Nodes.expr list) : (Nodes.expr array, error) result =
		match TokenQueue.peek tq with
		| Some t when Tokens.is_argument t ->
			begin match prefix_parser t with
			| Some parser ->
				begin match parser tq with
				| Ok arg -> parse_arguments (arg::args)
				| Error e -> Error e
				end
			(* This shouldn't ever trigger as all argument tokens should be given prefix
			 * parsers. If it does we're going to have some confused users.
			 *)
			| None -> Error (Printf.sprintf "Couldn't find prefix parser for argument token \"%s\""
				(Tokens.literal t), Tokens.location t)
			end
		| _ -> Ok (Array.of_list (List.rev args))
	in match parse_func () with
	| Ok func ->
		begin match TokenQueue.peek tq with
		| Some t when Tokens.is_argument t ->
			begin match parse_arguments [] with
			| Ok args -> Ok (Application (func, args, Nodes.location func))
			| Error e -> Error e
			end
		| _ -> Ok func
		end
	| Error e -> Error e

and parse_expression (tq : TokenQueue.t) (prec : int) : (Nodes.expr, error) result =
	let rec parse_infix (left : Nodes.expr) : (Nodes.expr, error) result =
		match TokenQueue.peek tq with
		| Some t when precedence t > prec ->
			begin match infix_parser t with
			| Some parser ->
				begin match parser tq left with
				| Ok left -> parse_infix left
				| Error e -> Error e
				end
			| None -> Ok left
			end
		| _ -> Ok left
	in match parse_application tq with
	| Ok left ->
		parse_infix left
	| Error e -> Error e

and parse_let_definition (tq : TokenQueue.t) : (Nodes.def, error) result =
	let parse_assignment () : (Nodes.expr, error) result =
		match TokenQueue.next tq with
		| Some (Equals _) ->
			begin match parse_expression tq 0 with
			| Ok expr -> Ok expr
			| Error e -> Error e
			end
		| Some t -> Error (unexpected_token t "an assignment")
		| None -> Error (unexpected_eof tq.location "an assignment")
	in let rec parse_arguments (args : string list) : string array =
		match TokenQueue.peek tq with
		| Some (Identifier (arg, _)) -> TokenQueue.junk tq; parse_arguments (arg::args)
		| _ -> Array.of_list (List.rev args)
	in match TokenQueue.npeek 2 tq with
	| [Let l; Identifier (i, _)] ->
		TokenQueue.njunk 2 tq;
		begin match parse_arguments [] with
		| [||] ->
			begin match parse_assignment () with
			| Ok expr -> Ok (Variable (i, expr, l))
			| Error e -> Error e
			end
		| args ->
			begin match parse_assignment () with
			| Ok expr -> Ok (Function (i, args, expr, l))
			| Error e -> Error e
			end
		end
	| [Let _; t] -> Error (unexpected_token t "an identifier")
	| (Let _)::[] -> Error (unexpected_eof tq.location "an identifier")
	| t::_ -> Error (unexpected_token t "let")
	| [] -> Error (unexpected_eof tq.location "let")

and parse_type_definition (tq : TokenQueue.t) : (Nodes.def, error) result =
	let rec parse_members (members : string list) : string array =
		match TokenQueue.npeek 2 tq with
		| [Identifier (member, _); Semicolon _] ->
			TokenQueue.njunk 2 tq; parse_members (member::members)
		| [Identifier (member, _); _] ->
			TokenQueue.junk tq; Array.of_list (List.rev (member::members))
		| _ -> Array.of_list (List.rev members)
	in let parse_struct () : (string array, error) result =
		match TokenQueue.next tq with
		| Some (LBrace _) ->
			let members = parse_members []
			in begin match TokenQueue.next tq with
			| Some (RBrace _) -> Ok members
			| Some t -> Error (unexpected_token t "a closing brace")
			| None -> Error (unexpected_eof tq.location " a closing brace")
			end
		| Some t -> Error (unexpected_token t "a struct")
		| None -> Error (unexpected_eof tq.location "a struct")
	in match TokenQueue.npeek 3 tq with
	| [Type l; Identifier (i, _); Equals _] ->
		TokenQueue.njunk 3 tq;
		begin match parse_struct () with
		| Ok members -> Ok (Type (i, members, l))
		| Error e -> Error e
		end
	| [Type _; Identifier _; t] -> Error (unexpected_token t "an assignment")
	| (Type _)::t::_ -> Error (unexpected_token t "an identifier")
	| t::_ -> Error (unexpected_token t "a type")
	| [] -> Error (unexpected_eof tq.location "a type")

and parse_definition (tq : TokenQueue.t) : (Nodes.def, error) result =
	match TokenQueue.peek tq with
	| Some (Let _) -> parse_let_definition tq
	| Some (Type _) -> parse_type_definition tq
	| Some t -> Error (unexpected_token t "a definition")
	| None -> Error (unexpected_eof tq.location "a definition")

let rec parse_statement (tq : TokenQueue.t) : (Nodes.statement option, error) result =
	match TokenQueue.peek tq with
	| Some (Let _) | Some (Type _) ->
		let def = parse_definition tq
		in begin match (def, TokenQueue.peek tq) with
		| (Ok def, Some (In l)) ->
			TokenQueue.junk tq;
			begin match parse_expression tq 0 with
			| Ok expr -> Ok (Some (Expression (Local (def, expr, l))))
			| Error e -> Error e
			end
		| (Ok def, _) -> Ok (Some (Definition def))
		| (Error e, _) -> Error e
		end
	| Some (Semicolon _) -> TokenQueue.junk tq; parse_statement tq
	| Some _ ->
		begin match parse_expression tq 0 with
		| Ok expr -> Ok (Some (Expression expr))
		| Error e -> Error e
		end
	| None -> Ok None

let parse_program (tq : TokenQueue.t) : (Nodes.program, error) result =
	let rec parse_until_eof (prog : Nodes.program) : (Nodes.program, error) result =
		match parse_statement tq with
		| Ok (Some s) -> parse_until_eof (s::prog)
		| Ok None -> Ok (List.rev prog)
		| Error e -> Error e
	in parse_until_eof []

let () : unit =
	let parse_print_program (tq : TokenQueue.t) : unit =
		match parse_program tq with
		| Ok prog -> Nodes.serialize_program stdout prog
		| Error (e, l) -> Printf.eprintf "Error at %s : %s\n" (Location.to_string l) e; exit 1
	in if Array.length Sys.argv > 1 then
		for i = 1 to Array.length Sys.argv - 1 do
			parse_print_program (TokenQueue.of_channel (open_in Sys.argv.(i)))
		done
	else
		parse_print_program (TokenQueue.of_channel stdin)
