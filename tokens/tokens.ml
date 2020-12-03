type t =
	| Comment of string * Location.t
	| LParen of Location.t
	| RParen of Location.t
	| LBrace of Location.t (* { *)
	| RBrace of Location.t (* } *)
	| LBracket of Location.t (* [ *)
	| RBracket of Location.t (* ] *)
	| Arrow of Location.t (* -> *)
	| Semicolon of Location.t
	| Bar of Location.t (* | *)
	| String of string * Location.t
	| Float of float * Location.t
	| Int of int * Location.t
	| Identifier of string * Location.t
	(* Operators *)
	| Equals of Location.t
	| LessThan of Location.t
	| GreaterThan of Location.t
	| LessThanEqual of Location.t
	| GreaterThanEqual of Location.t
	| Add of Location.t
	| Subtract of Location.t
	| Multiply of Location.t 
	| Divide of Location.t
	| And of Location.t
	| Or of Location.t
	| Not of Location.t
	(* Keywords *)
	| Let of Location.t
	| In of Location.t
	| Cond of Location.t
	| Of of Location.t
	| If of Location.t
	| Fun of Location.t
	| Type of Location.t
	| Then of Location.t
	| Else of Location.t

let location (token : t) : Location.t =
	match token with
	| Comment (_, l) -> l
	| LParen l -> l
	| RParen l -> l
	| LBrace l -> l
	| RBrace l -> l
	| LBracket l -> l
	| RBracket l -> l
	| Arrow l -> l
	| Semicolon l -> l
	| Bar l -> l
	| String (_, l) -> l
	| Float (_, l) -> l
	| Int (_, l) -> l
	| Identifier (_, l) -> l
	(* Operators *)
	| Equals l -> l
	| LessThan l -> l
	| GreaterThan l -> l
	| LessThanEqual l -> l
	| GreaterThanEqual l -> l
	| Add l -> l
	| Subtract l -> l
	| Multiply l -> l
	| Divide l -> l
	| And l -> l
	| Or l -> l
	| Not l -> l
	(* Keywords *)
	| Let l -> l
	| In l -> l
	| Cond l -> l
	| Of l -> l
	| If l -> l
	| Fun l -> l
	| Type l -> l
	| Then l -> l
	| Else l -> l

let is_same_kind (token_1 : t) (token_2 : t) : bool =
	match (token_1, token_2) with
	| (Comment _, Comment _)
	| (LParen _, LParen _)
	| (RParen _, RParen _)
	| (LBrace _, LBrace _)
	| (RBrace _, RBrace _)
	| (LBracket _, LBracket _)
	| (RBracket _, RBracket _)
	| (Arrow _, Arrow _)
	| (Semicolon _, Semicolon _)
	| (Bar _, Bar _)
	| (String _, String _)
	| (Float _, Float _)
	| (Int _, Int _)
	| (Identifier _, Identifier _)
	(* Operators *)
	| (Equals _, Equals _)
	| (LessThan _, LessThan _)
	| (GreaterThan _, GreaterThan _)
	| (LessThanEqual _, LessThanEqual _)
	| (GreaterThanEqual _, GreaterThanEqual _)
	| (Add _, Add _)
	| (Subtract _, Subtract _)
	| (Multiply _, Multiply _)
	| (Divide _, Divide _)
	| (And _, And _)
	| (Or _, Or _)
	| (Not _, Not _)
	(* Keywords *)
	| (Let _, Let _)
	| (In _, In _)
	| (Cond _, Cond _)
	| (Of _, Of _)
	| (If _, If _)
	| (Fun _, Fun _)
	| (Type _, Type _)
	| (Then _, Then _)
	| (Else _, Else _)
		-> true
	| _ -> false

let is_comment (token : t) : bool =
	match token with
	| Comment _ -> true
	| _ -> false

let is_argument (token : t) : bool =
	match token with
	| String _ -> true
	| Float _ -> true
	| Int _ -> true
	| Identifier _ -> true
	| LParen _ -> true
	| LBracket _ -> true
	| _ -> false

let is_infix_operator (token : t) : bool =
	match token with
	| Equals _ -> true
	| LessThan _ -> true
	| GreaterThan _ -> true
	| LessThanEqual _ -> true
	| GreaterThanEqual _ -> true
	| Add _ -> true
	| Subtract _ -> true
	| Multiply _ -> true
	| Divide _ -> true
	| And _ -> true
	| Or _ -> true
	| _ -> false

let is_prefix_operator (token : t) : bool =
	match token with
	| Equals _ -> true
	| LessThan _ -> true
	| GreaterThan _ -> true
	| LessThanEqual _ -> true
	| GreaterThanEqual _ -> true
	| Add _ -> true
	| Subtract _ -> true
	| Multiply _ -> true
	| Divide _ -> true
	| And _ -> true
	| Or _ -> true
	| Not _ -> true
	| _ -> false

let to_string (token : t) : string =
	match token with
	| Comment (s, l) -> Printf.sprintf "Comment %s %s" (String.escaped s) (Location.to_string l)
	| LParen l -> Printf.sprintf "LParen %s" (Location.to_string l)
	| RParen l -> Printf.sprintf "RParen %s" (Location.to_string l)
	| LBrace l -> Printf.sprintf "LBrace %s" (Location.to_string l)
	| RBrace l -> Printf.sprintf "RBrace %s" (Location.to_string l)
	| LBracket l -> Printf.sprintf "LBracket %s" (Location.to_string l)
	| RBracket l -> Printf.sprintf "RBracket %s" (Location.to_string l)
	| Arrow l -> Printf.sprintf "Arrow %s" (Location.to_string l)
	| Semicolon l -> Printf.sprintf "Semicolon %s" (Location.to_string l)
	| Bar l -> Printf.sprintf "Bar %s" (Location.to_string l)
	| String (s, l) -> Printf.sprintf "String %s %s" (String.escaped s) (Location.to_string l)
	| Float (f, l) -> Printf.sprintf "Float %e %s" f (Location.to_string l)
	| Int (i, l) -> Printf.sprintf "Int %d %s" i (Location.to_string l)
	| Identifier (s, l) -> Printf.sprintf "Identifier %s %s" s (Location.to_string l)
	(* Operators *)
	| Equals l -> Printf.sprintf "Equals %s" (Location.to_string l)
	| LessThan l -> Printf.sprintf "LessThan %s" (Location.to_string l)
	| GreaterThan l -> Printf.sprintf "GreaterThan %s" (Location.to_string l)
	| LessThanEqual l -> Printf.sprintf "LessThanEqual %s" (Location.to_string l)
	| GreaterThanEqual l -> Printf.sprintf "GreaterThanEqual %s" (Location.to_string l)
	| Add l -> Printf.sprintf "Add %s" (Location.to_string l)
	| Subtract l -> Printf.sprintf "Subtract %s" (Location.to_string l)
	| Multiply l -> Printf.sprintf "Multiply %s" (Location.to_string l)
	| Divide l -> Printf.sprintf "Divide %s" (Location.to_string l)
	| And l -> Printf.sprintf "And %s" (Location.to_string l)
	| Or l -> Printf.sprintf "Or %s" (Location.to_string l)
	| Not l -> Printf.sprintf "Not %s" (Location.to_string l)
	(* Keywords *)
	| Let l -> Printf.sprintf "Let %s" (Location.to_string l)
	| In l -> Printf.sprintf "In %s" (Location.to_string l)
	| Cond l -> Printf.sprintf "Cond %s" (Location.to_string l)
	| Of l -> Printf.sprintf "Of %s" (Location.to_string l)
	| If l -> Printf.sprintf "If %s" (Location.to_string l)
	| Fun l -> Printf.sprintf "Fun %s" (Location.to_string l)
	| Type l -> Printf.sprintf "Type %s" (Location.to_string l)
	| Then l -> Printf.sprintf "Then %s" (Location.to_string l)
	| Else l -> Printf.sprintf "Else %s" (Location.to_string l)

(* I love kakoune macros *)
let of_string (s : string) : t option =
	let rec split_last_three (three : string list) (rest : string list) : (string list) * (string list) =
		match three with
		| [_; _; _] | [_; _] | [_] | [] -> (List.rev rest, three)
		| hd::tl -> split_last_three tl (hd::rest)
	in match String.split_on_char ' ' s with
	| "Comment"::tl ->
		let (s, l) = split_last_three tl []
		in begin match Location.of_string_list l with
		| Some l -> Some (Comment (Scanf.unescaped (String.concat " " s), l))
		| None -> None
		end
	| "LParen"::tl ->
		begin match Location.of_string_list tl with
		| Some l -> Some (LParen l)
		| None -> None
		end
	| "RParen"::tl ->
		begin match Location.of_string_list tl with
		| Some l -> Some (RParen l)
		| None -> None
		end
	| "String"::tl -> 
		let (s, l) = split_last_three tl []
		in begin match Location.of_string_list l with
		| Some l -> Some (String (Scanf.unescaped (String.concat " " s), l))
		| None -> None
		end
	| "Int"::i::tl ->
		begin match (Location.of_string_list tl, int_of_string_opt i) with
		| (None, _) -> None
		| (_, None) -> None
		| (Some l, Some i) -> Some (Int (i, l))
		end
	| "Float"::f::tl ->
		begin match (Location.of_string_list tl, float_of_string_opt f) with
		| (None, _) -> None
		| (_, None) -> None
		| (Some l, Some f) -> Some (Float (f, l))
		end
	| "Identifier"::a::tl ->
		begin match Location.of_string_list tl with
		| Some l -> Some (Identifier (a, l))
		| None -> None
		end
	| "LBrace"::tl ->
		begin match Location.of_string_list tl with
		| Some l -> Some (LBrace l)
		| None -> None
		end
	| "RBrace"::tl ->
		begin match Location.of_string_list tl with
		| Some l -> Some (RBrace l)
		| None -> None
		end
	| "LBracket"::tl ->
		begin match Location.of_string_list tl with
		| Some l -> Some (LBracket l)
		| None -> None
		end
	| "RBracket"::tl ->
		begin match Location.of_string_list tl with
		| Some l -> Some (RBracket l)
		| None -> None
		end
	| "Arrow"::tl ->
		begin match Location.of_string_list tl with
		| Some l -> Some (Arrow l)
		| None -> None
		end
	| "Semicolon"::tl ->
		begin match Location.of_string_list tl with
		| Some l -> Some (Semicolon l)
		| None -> None
		end
	| "Bar"::tl ->
		begin match Location.of_string_list tl with
		| Some l -> Some (Bar l)
		| None -> None
		end
	(* Operators *)
	| "Equals"::tl ->
		begin match Location.of_string_list tl with
		| Some l -> Some (Equals l)
		| None -> None
		end
	| "LessThan"::tl ->
		begin match Location.of_string_list tl with
		| Some l -> Some (LessThan l)
		| None -> None
		end
	| "GreaterThan"::tl ->
		begin match Location.of_string_list tl with
		| Some l -> Some (GreaterThan l)
		| None -> None
		end
	| "LessThanEqual"::tl ->
		begin match Location.of_string_list tl with
		| Some l -> Some (LessThanEqual l)
		| None -> None
		end
	| "GreaterThanEqual"::tl ->
		begin match Location.of_string_list tl with
		| Some l -> Some (GreaterThanEqual l)
		| None -> None
		end
	| "Add"::tl ->
		begin match Location.of_string_list tl with
		| Some l -> Some (Add l)
		| None -> None
		end
	| "Subtract"::tl ->
		begin match Location.of_string_list tl with
		| Some l -> Some (Subtract l)
		| None -> None
		end
	| "Multiply"::tl ->
		begin match Location.of_string_list tl with
		| Some l -> Some (Multiply l)
		| None -> None
		end
	| "Divide"::tl ->
		begin match Location.of_string_list tl with
		| Some l -> Some (Divide l)
		| None -> None
		end
	| "And"::tl ->
		begin match Location.of_string_list tl with
		| Some l -> Some (And l)
		| None -> None
		end
	| "Or"::tl ->
		begin match Location.of_string_list tl with
		| Some l -> Some (Or l)
		| None -> None
		end
	| "Not"::tl ->
		begin match Location.of_string_list tl with
		| Some l -> Some (Not l)
		| None -> None
		end
	(* Keywords *)
	| "Let"::tl ->
		begin match Location.of_string_list tl with
		| Some l -> Some (Let l)
		| None -> None
		end
	| "In"::tl ->
		begin match Location.of_string_list tl with
		| Some l -> Some (In l)
		| None -> None
		end
	| "Cond"::tl ->
		begin match Location.of_string_list tl with
		| Some l -> Some (Cond l)
		| None -> None
		end
	| "Of"::tl ->
		begin match Location.of_string_list tl with
		| Some l -> Some (Of l)
		| None -> None
		end
	| "If"::tl ->
		begin match Location.of_string_list tl with
		| Some l -> Some (If l)
		| None -> None
		end
	| "Fun"::tl ->
		begin match Location.of_string_list tl with
		| Some l -> Some (Fun l)
		| None -> None
		end
	| "Type"::tl ->
		begin match Location.of_string_list tl with
		| Some l -> Some (Type l)
		| None -> None
		end
	| "Then"::tl ->
		begin match Location.of_string_list tl with
		| Some l -> Some (Then l)
		| None -> None
		end
	| "Else"::tl ->
		begin match Location.of_string_list tl with
		| Some l -> Some (Else l)
		| None -> None
		end
	| _ -> None

let literal (token : t) : string =
	match token with
	| Comment (s, _) -> Scanf.unescaped s
	| LParen _ -> "("
	| RParen _ -> ")"
	| LBrace _ -> "{"
	| RBrace _ -> "}"
	| LBracket _ -> "["
	| RBracket _ -> "]"
	| Arrow _ -> "->"
	| Semicolon _ -> ";"
	| Bar _ -> "|"
	| String (s, _) -> s
	| Float (f, _) -> string_of_float f
	| Int (i, _) -> string_of_int i
	| Identifier (s, _) -> s
	(* Operators *)
	| Equals _ -> "="
	| LessThan _ -> "<"
	| GreaterThan _ -> ">"
	| LessThanEqual _ -> "<="
	| GreaterThanEqual _ -> ">="
	| Add _ -> "+"
	| Subtract _ -> "-"
	| Multiply _ -> "*"
	| Divide _ -> "/"
	| And _ -> "and"
	| Or _ -> "or"
	| Not _ -> "!"
	(* Keywords *)
	| Let _ -> "let"
	| In _ -> "in"
	| Cond _ -> "cond"
	| Of _ -> "of"
	| If _ -> "if"
	| Fun _ -> "fun"
	| Type _ -> "type"
	| Then _ -> "then"
	| Else _ -> "else"
