let s_of_op (op : Nodes.op) : Sexpr.t =
	match op with
	| Add -> Symbol "+"
	| Subtract -> Symbol "-"
	| Multiply -> Symbol "*"
	| Divide -> Symbol "/"
	| And -> Symbol "and"
	| Or -> Symbol "or"
	| Not -> Symbol "not"
	| Equals -> Symbol "="
	| GreaterThan -> Symbol ">"
	| LessThan -> Symbol "<"
	| GreaterThanEqual -> Symbol ">="
	| LessThanEqual -> Symbol "<="

let rec s_of_expr (expr : Nodes.expr) : Sexpr.t =
	match expr with
	| Int (i, _) -> Int i
	| Float (f, _) -> Float f
	| String (s, _) -> String s
	| Identifier (n, _) -> Symbol n
	| Operator (op, left, right, _) ->
		ListP [s_of_op op; s_of_expr left; s_of_expr right]
	| Application (func, args, _) ->
		ListP ((s_of_expr func)::(Array.to_list (Array.map s_of_expr args)))
	| List (elements, _) ->
		ListP ((Symbol "list")::(Array.to_list (Array.map s_of_expr elements)))
	| Lambda (args, body, _) ->
		ListP [(Symbol "lambda"); (ListP (Array.to_list (Array.map Sexpr.symbol args))); (s_of_expr body)]
	| Local (def, body, _) ->
		ListP [Symbol "local"; ListB [s_of_def def]; s_of_expr body]
	| Cond (cases, _) ->
		ListP ((Symbol "cond")
			::(Array.to_list (Array.map (fun (cond, cons) : Sexpr.t ->
				ListB [s_of_expr cond; s_of_expr cons]) cases)))
	| If (cond, cons, alt, _) ->
		ListP [Symbol "if"; s_of_expr cond; s_of_expr cons; s_of_expr alt]

and s_of_def (def : Nodes.def) : Sexpr.t =
	match def with
	| Variable (name, value, _) -> ListP [Symbol "define"; Symbol name; s_of_expr value]
	| Function (name, args, body, _) ->
		ListP [Symbol "define";
			ListP ((Symbol name)::(Array.to_list (Array.map Sexpr.symbol args)));
			s_of_expr body]
	| Type (name, members, _) ->
		ListP [Symbol "define-struct"; Symbol name; ListP (Array.to_list (Array.map Sexpr.symbol members))]

let fprintf_statement (oc : out_channel) (s : Nodes.statement) : unit =
	match s with
	| Definition def -> Printf.fprintf oc "%s\n" (Sexpr.pretty_print (s_of_def def))
	| Expression expr -> Printf.fprintf oc "%s\n" (Sexpr.pretty_print (s_of_expr expr))

let fprintf_program (oc : out_channel) (prog : Nodes.program) : unit =
	let rec loop (prog : Nodes.program) : unit =
		match prog with
		| [s] -> fprintf_statement oc s
		| s::tl -> fprintf_statement oc s; Printf.fprintf oc "\n\n\n"; loop tl
		| [] -> ()
	in Printf.fprintf oc "#reader(lib \"htdp-advanced-reader.ss\" \"lang\")
	((modname example2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings 
	 #(#t constructor repeating-decimal #f #t none #f () #t)))\n"; (* advanced student preamble *)
	loop prog

let () : unit =
	let deserialize_fprint_program (ic : in_channel) (oc : out_channel) : unit =
		match Nodes.deserialize_program ic with
		| Ok prog -> fprintf_program oc prog
		| Error e -> Printf.eprintf "Error during deserialization : %s\n" e; exit 1
	in if Array.length Sys.argv > 1 then
		for i = 1 to Array.length Sys.argv - 1 do
			deserialize_fprint_program (open_in Sys.argv.(i)) stdout
		done
	else
		deserialize_fprint_program stdin stdout
