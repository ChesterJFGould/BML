type t =
	| Symbol of string
	| String of string
	| Int of int
	| Float of float
	| ListP of t list
	| ListB of t list

let symbol (s : string) : t = Symbol s

let pretty_print (expr : t) : string =
	let indent_n (n : int) : string = String.make n ' '
	in let rec print (expr : t) (indent : int) : string * int =
		let rec print_elements (elements : t list) (prev : string) (indent : int) (max_l : int)
		: string * int =
			match elements with
			| [] -> ("", 0)
			| [e] ->
				let (s, width) = print e indent
				in (Printf.sprintf "%s%s%s" prev (indent_n indent) s, max max_l width)
			| e::tl ->
				let (s, width) = print e indent
				in print_elements
					tl (Printf.sprintf "%s%s%s\n" prev (indent_n indent) s) indent
					(max max_l width)
		in let print_list (delim_l : char) (delim_r : char) (elements : t list) =
			match elements with
			| [] -> ((String.make 1 delim_l) ^ (String.make 1 delim_r), 2)
			| [e] ->
				let (s, width) = print e indent
				in (Printf.sprintf "%c%s%c" delim_l s delim_r, width + 2)
			| [first; second] ->
				let (first, first_width) = print first (indent + 1)
				in let new_indent = indent + first_width + 2
				in let (second, second_width) = print second new_indent
				in (Printf.sprintf "%c%s %s%c" delim_l first second delim_r, first_width + second_width + 3)
			| first::second::rest ->
				let (first, first_width) = print first (indent + 1)
				in let new_indent = indent + first_width + 2
				in let (second, second_width) = print second new_indent
				in let (rest, rest_width) = print_elements rest "" new_indent 0
				in (Printf.sprintf "%c%s %s\n%s%c" delim_l first second rest delim_r
					, first_width + (max second_width rest_width) + 3)
		in match expr with
		| Symbol s -> (s, String.length s)
		| String s -> (Printf.sprintf "\"%s\"" s, String.length s + 2)
		| Int i -> let s = string_of_int i in (s, String.length s)
		| Float f -> let s = string_of_float f in (s, String.length s)
		| ListP elements -> print_list '(' ')' elements
		| ListB elements -> print_list '[' ']' elements
	in let (s, _) = print expr 0 in s
