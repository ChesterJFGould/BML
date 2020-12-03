type op =
	| Add
	| Subtract
	| Multiply
	| Divide
	| And
	| Or
	| Not
	| Equals
	| GreaterThan
	| LessThan
	| GreaterThanEqual
	| LessThanEqual

let string_of_op (op : op) : string =
	match op with
	| Add -> "Add"
	| Subtract -> "Subtract"
	| Multiply -> "Multiply"
	| Divide -> "Divide"
	| And -> "And"
	| Or -> "Or"
	| Not -> "Not"
	| Equals -> "Equals"
	| GreaterThan -> "GreaterThan"
	| LessThan -> "LessThan"
	| GreaterThanEqual -> "GreaterThanEqual"
	| LessThanEqual -> "LessThanEqual"
	
let op_of_string (s : string) : op option =
	match s with
	| "Add" -> Some Add
	| "Subtract" -> Some Subtract
	| "Multiply" -> Some Multiply
	| "Divide" -> Some Divide
	| "And" -> Some And
	| "Or" -> Some Or
	| "Not" -> Some Not
	| "Equals" -> Some Equals
	| "GreaterThan" -> Some GreaterThan
	| "LessThan" -> Some LessThan
	| "GreaterThanEqual" -> Some GreaterThanEqual
	| "LessThanEqual" -> Some LessThanEqual
	| _ -> None

let op_literal (op : op) : string =
	match op with
	| Add -> "+"
	| Subtract -> "-"
	| Multiply -> "*"
	| Divide -> "/"
	| And -> "and"
	| Or -> "or"
	| Not -> "!"
	| Equals -> "="
	| GreaterThan -> ">"
	| LessThan -> "<"
	| GreaterThanEqual -> ">="
	| LessThanEqual -> "<="

type def =
	| Variable of string * expr * Location.t
	| Function of string * (string array) * expr * Location.t
	| Type of string * (string array) * Location.t


and expr =
	| Int of int * Location.t
	| Float of float * Location.t
	| String of string * Location.t
	| Identifier of string * Location.t
	| Operator of op * expr * expr * Location.t
	| Application of expr * (expr array) * Location.t
	| List of (expr array) * Location.t
	| Lambda of (string array) * expr * Location.t
	| Local of def * expr * Location.t
	| Cond of ((expr * expr) array) * Location.t
	| If of expr * expr * expr * Location.t

let location (expr : expr) : Location.t =
	match expr with
	| Int (_, l) -> l
	| Float (_, l) -> l
	| String (_, l) -> l
	| Identifier (_, l) -> l
	| Operator (_, _, _, l) -> l
	| Application (_, _, l) -> l
	| List (_, l) -> l
	| Lambda (_, _, l) -> l
	| Local (_, _, l) -> l
	| Cond (_, l) -> l
	| If (_, _, _, l) -> l
	
let rec serialize_def (oc : out_channel) (def : def) : unit =
	match def with
	| Variable (name, expr, l) ->
		Printf.fprintf oc "Variable %s %s\n" name (Location.to_string l);
		serialize_expr oc expr
	| Function (name, args, expr, l) ->
		Printf.fprintf oc "Function %s %s %s\n"
			name (String.concat " " (Array.to_list args)) (Location.to_string l);
		serialize_expr oc expr
	| Type (name, members, l) ->
		Printf.fprintf oc "Type %s %s %s\n"
			name (String.concat " " (Array.to_list members)) (Location.to_string l)

and serialize_expr (oc : out_channel) (expr : expr) : unit =
	match expr with
	| Int (i, l) -> Printf.fprintf oc "Int %d %s\n" i (Location.to_string l)
	| Float (f, l) -> Printf.fprintf oc "Float %f %s\n" f (Location.to_string l)
	| String (s, l) -> Printf.fprintf oc "String %s %s\n" (String.escaped s) (Location.to_string l)
	| Identifier (n, l) -> Printf.fprintf oc "Identifier %s %s\n" n (Location.to_string l)
	| Operator (op, left, right, l) ->
		Printf.fprintf oc "Operator %s %s\n" (string_of_op op) (Location.to_string l);
		serialize_expr oc left;
		serialize_expr oc right
	| Application (func, args, l) ->
		Printf.fprintf oc "Application %d %s\n" (Array.length args) (Location.to_string l);
		serialize_expr oc func;
		Array.iter (serialize_expr oc) args
	| List (elements, l) ->
		Printf.fprintf oc "List %d %s\n" (Array.length elements) (Location.to_string l);
		Array.iter (serialize_expr oc) elements
	| Lambda (args, body, l) ->
		Printf.fprintf oc "Lambda %s %s\n"
			(String.concat " " (Array.to_list args)) (Location.to_string l);
		serialize_expr oc body
	| Local (def, expr, l) ->
		Printf.fprintf oc "Local %s\n" (Location.to_string l);
		serialize_def oc def;
		serialize_expr oc expr
	| Cond (cases, l) ->
		Printf.fprintf oc "Cond %d %s\n" (Array.length cases) (Location.to_string l);
		Array.iter (fun (f, t) -> serialize_expr oc f; serialize_expr oc t) cases
	| If (cond, cons, alt, l) ->
		Printf.fprintf oc "If %s\n" (Location.to_string l);
		serialize_expr oc cond;
		serialize_expr oc cons;
		serialize_expr oc alt

let rec deserialize_def (ic : in_channel) : (def, string) result =
	let rec split_last_three (three : string list) (rest : string list) : (string list) * (string list) =
		match three with
		| [_; _; _] | [_; _] | [_] | [] -> (List.rev rest, three)
		| hd::tl -> split_last_three tl (hd::rest)
	in let invalid_loc_list (l : string list) : string =
		Printf.sprintf "couldn't parse location from %s" (String.concat " " l)
	in match String.split_on_char ' ' (input_line ic) with
	| "Variable"::name::tl ->
		let l = Location.of_string_list tl
		in let value = deserialize_expr ic
		in begin match (l, value) with
		| (Some l, Ok value) -> Ok (Variable (name, value, l))
		| (None, _) -> Error (invalid_loc_list tl)
		| (_, Error e) -> Error e
		end
	| "Function"::name::tl ->
		let (args, tl) = split_last_three tl []
		in let l = Location.of_string_list tl
		in let body = deserialize_expr ic
		in begin match (l, body) with
		| (Some l, Ok body) -> Ok (Function (name, Array.of_list args, body, l))
		| (None, _) -> Error (invalid_loc_list tl)
		| (_, Error e) -> Error e
		end
	| "Type"::name::tl ->
		let (members, tl) = split_last_three tl []
		in begin match Location.of_string_list tl with
		| Some l -> Ok (Type (name, Array.of_list members, l))
		| None -> Error (invalid_loc_list tl)
		end
	| s -> Error (Printf.sprintf "cannot deserialize %S into a definition" (String.concat " " s))

and deserialize_expr (ic : in_channel) : (expr, string) result =
	let rec split_last_three (three : string list) (rest : string list) : (string list) * (string list) =
		match three with
		| [_; _; _] | [_; _] | [_] | [] -> (List.rev rest, three)
		| hd::tl -> split_last_three tl (hd::rest)
	in let invalid_loc_list (l : string list) : string =
		Printf.sprintf "couldn't parse location from %s" (String.concat " " l)
	in match String.split_on_char ' ' (input_line ic) with
	| "Int"::i::tl ->
		begin match (int_of_string_opt i, Location.of_string_list tl) with
		| (Some i, Some l) -> Ok (Int (i, l))
		| (None, _) -> Error (Printf.sprintf "cannot parse %s into int" i)
		| (_, None) -> Error (invalid_loc_list tl)
		end
	| "Float"::f::tl ->
		begin match (float_of_string_opt f, Location.of_string_list tl) with
		| (Some f, Some l) -> Ok (Float (f, l))
		| (None, _) -> Error (Printf.sprintf "cannot parse %s into float" f)
		| (_, None) -> Error (invalid_loc_list tl)
		end
	| "String"::tl ->
		let (s, tl) = split_last_three tl []
		in begin match Location.of_string_list tl with
		| Some l -> Ok (String (Scanf.unescaped (String.concat " " s), l))
		| None -> Error (invalid_loc_list tl)
		end
	| "Identifier"::n::tl ->
		begin match Location.of_string_list tl with
		| Some l -> Ok (Identifier (n, l))
		| None -> Error (invalid_loc_list tl)
		end
	| "Operator"::op::tl ->
		let left = deserialize_expr ic
		in let right = deserialize_expr ic
		in begin match (op_of_string op, Location.of_string_list tl, left, right) with
		| (Some op, Some l, Ok left, Ok right) -> Ok (Operator (op, left, right, l))
		| (None, _, _, _) -> Error (Printf.sprintf "cannot parse %s into op" op)
		| (_, None, _, _) -> Error (invalid_loc_list tl)
		| (_, _, Error e, _) -> Error e
		| (_, _, _, Error e) -> Error e
		end
	| "Application"::n::tl ->
		let rec deserialize_args (n : int) (args : expr list) : (expr array, string) result =
			if n = 0 then
				Ok (Array.of_list (List.rev args))
			else match deserialize_expr ic with
			| Ok arg -> deserialize_args (pred n) (arg::args)
			| Error e -> Error e
		in begin match int_of_string_opt n with
		| Some n ->
			let func = deserialize_expr ic
			in let args = deserialize_args n []
			in begin match (Location.of_string_list tl, func ,args) with
			| (Some l, Ok func, Ok args) -> Ok (Application (func, args, l))
			| (None, _, _) -> Error (invalid_loc_list tl)
			| (_, Error e, _) -> Error e
			| (_, _, Error e) -> Error e
			end
		| None -> Error (Printf.sprintf "cannot parse %s into int" n)
		end
	| "List"::n::tl ->
		let rec deserialize_elements (n : int) (args : expr list) : (expr array, string) result =
			if n = 0 then
				Ok (Array.of_list (List.rev args))
			else match deserialize_expr ic with
			| Ok arg -> deserialize_elements (pred n) (arg::args)
			| Error e -> Error e
		in begin match int_of_string_opt n with
		| Some n ->
			begin match (Location.of_string_list tl, deserialize_elements n []) with
			| (Some l, Ok elements) -> Ok (List (elements, l))
			| (None, _) -> Error (invalid_loc_list tl)
			| (_, Error e) -> Error e
			end
		| None -> Error (Printf.sprintf "cannot parse %s into int" n)
		end
	| "Lambda"::tl ->
		let (args, tl) = split_last_three tl []
		in begin match (Location.of_string_list tl, deserialize_expr ic) with
		| (Some l, Ok body) -> Ok (Lambda (Array.of_list args, body, l))
		| (None, _) -> Error (invalid_loc_list tl)
		| (_, Error e) -> Error e
		end
	| "Local"::tl ->
		let def = deserialize_def ic
		in let body = deserialize_expr ic
		in begin match (Location.of_string_list tl, def, body) with
		| (Some l, Ok def, Ok body) -> Ok (Local (def, body, l))
		| (None, _, _) -> Error (invalid_loc_list tl)
		| (_, Error e, _) -> Error e
		| (_, _, Error e) -> Error e
		end
	| "Cond"::n::tl ->
		let rec deserialize_cases (n : int) (cases : (expr * expr) list)
		: ((expr * expr) array, string) result =
			if n = 0 then
				Ok (Array.of_list (List.rev cases))
			else
				let cond = deserialize_expr ic
				in let cons = deserialize_expr ic
				in match (cond, cons) with
				| (Ok cond, Ok cons) -> deserialize_cases (pred n) ((cond, cons)::cases)
				| (Error e, _) -> Error e
				| (_, Error e) -> Error e
		in begin match int_of_string_opt n with
		| Some n ->
			begin match (Location.of_string_list tl, deserialize_cases n []) with
			| (Some l, Ok cases) -> Ok (Cond (cases, l))
			| (None, _) -> Error (invalid_loc_list tl)
			| (_, Error e) -> Error e
			end
		| None -> Error (Printf.sprintf "cannot parse %s into int" n)
		end
	| "If"::tl ->
		let cond = deserialize_expr ic
		in let cons = deserialize_expr ic
		in let alt = deserialize_expr ic
		in begin match (Location.of_string_list tl, cond, cons, alt) with
		| (Some l, Ok cond, Ok cons, Ok alt) -> Ok (If (cond, cons, alt, l))
		| (None, _, _, _) -> Error (invalid_loc_list tl)
		| (_, Error e, _, _) -> Error e
		| (_, _, Error e, _) -> Error e
		| (_, _, _, Error e) -> Error e
		end
	| s -> Error (Printf.sprintf "cannot deserialize %S into an expression" (String.concat " " s))

type statement =
	| Definition of def
	| Expression of expr

let serialize_statement (oc : out_channel) (s : statement) : unit =
	match s with
	| Definition def ->
		Printf.fprintf oc "Definition\n";
		serialize_def oc def
	| Expression expr ->
		Printf.fprintf oc "Expression\n";
		serialize_expr oc expr

let deserialize_statement (ic : in_channel) : (statement, string) result =
	match input_line ic with
	| "Definition" ->
		begin match deserialize_def ic with
		| Ok def -> Ok (Definition def)
		| Error e -> Error e
		end
	| "Expression" ->
		begin match deserialize_expr ic with
		| Ok expr -> Ok (Expression expr)
		| Error e -> Error e
		end
	| s -> Error (Printf.sprintf "cannot deserialize %S into a statement" s)
	
type program = statement list

let rec serialize_program (oc : out_channel) (prog : program) : unit =
	match prog with
	| s::tl ->
		serialize_statement oc s;
		serialize_program oc tl
	| [] -> ()

let deserialize_program (ic : in_channel) : (program, string) result =
	let rec loop (prog : program) : (program, string) result =
		try
			match deserialize_statement ic with
			| Ok s -> loop (s::prog)
			| Error e -> Error e
		with End_of_file -> Ok (List.rev prog)
	in loop []

let fprintf_tree (oc : out_channel) (prog : program) =
	let next_indent (indent : string) (last : bool) =
		if last then begin
			Printf.fprintf oc "\\-";
			indent ^ "  "
		end else begin
			Printf.fprintf oc "|-";
			indent ^ "| "
		end
	in let rec expr_tree (indent : string) (last : bool) (expr : expr) : unit =
		output_string oc indent;
		let new_indent = next_indent indent last
		in match expr with
		| Int (i, _) -> Printf.fprintf oc "%d\n" i
		| Float (f, _) -> Printf.fprintf oc "%f\n" f
		| String (s, _) -> Printf.fprintf oc "%S\n" s
		| Identifier (n, _) -> Printf.fprintf oc "%s\n" n
		| Operator (op, left, right, _) ->
			Printf.fprintf oc "(%s)\n" (op_literal op);
			expr_tree new_indent false left ;
			expr_tree new_indent true right;
		| Application (func, args, _) ->
			Printf.fprintf oc "@\n";
			expr_tree new_indent false func;
			let last = Array.length args - 1
			in Array.iteri (fun i arg -> expr_tree new_indent (i = last) arg) args
		| List (elements, _) ->
			Printf.fprintf oc "list\n";
			let last = Array.length elements - 1
			in Array.iteri (fun i element -> expr_tree new_indent (i = last) element) elements
		| Lambda (args, body, _) ->
			Printf.fprintf oc "fun %s\n" (String.concat " " (Array.to_list (args)));
			expr_tree new_indent true body;
		| Local (def, body, _) ->
			Printf.fprintf oc "in\n";
			def_tree new_indent false def;
			expr_tree new_indent true body
		| Cond (cases, _) ->
			Printf.fprintf oc "cond\n";
			let case_tree (indent : string) (last : bool) ((cond, cons) : expr * expr) : unit =
				output_string oc indent;
				let new_indent = next_indent indent last
				in Printf.fprintf oc "->\n";
				expr_tree new_indent false cond;
				expr_tree new_indent true cons
			in let last = Array.length cases - 1
			in Array.iteri (fun i case -> case_tree new_indent (i = last) case) cases
		| If (cond, cons, alt, _) ->
			Printf.fprintf oc "if\n";
			expr_tree new_indent false cond;
			expr_tree new_indent false cons;
			expr_tree new_indent true alt
	and def_tree (indent : string) (last : bool) (def : def) : unit =
		output_string oc indent;
		let new_indent = next_indent indent last
		in match def with
		| Variable (name, value, _) ->
			Printf.fprintf oc "let %s\n" name;
			expr_tree new_indent true value
		| Function (name, args, body, _) ->
			Printf.fprintf oc "let %s %s\n" name (String.concat " " (Array.to_list args));
			expr_tree new_indent true body
		| Type (name, members, _) ->
			Printf.fprintf oc "type %s {%s}\n" name (String.concat "; " (Array.to_list members))
	and statement_tree (indent : string) (last : bool) (s : statement) : unit =
		output_string oc indent;
		let new_indent = next_indent indent last
		in match s with
		| Definition def ->
			Printf.fprintf oc "def\n";
			def_tree new_indent true def
		| Expression expr ->
			Printf.fprintf oc "expr\n";
			expr_tree new_indent true expr
	in let new_indent = next_indent "" true
	in Printf.fprintf oc "Program\n";
	let last = List.length prog - 1
	in List.iteri (fun i s -> statement_tree new_indent (i = last) s) prog
