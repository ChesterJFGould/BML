let () : unit =
	let print_tree (ic : in_channel) : unit =
		match Nodes.deserialize_program ic with
		| Ok prog -> Nodes.fprintf_tree stdout prog
		| Error e -> Printf.eprintf "Error during deserialization: %s\n" e; exit 1
	in if Array.length Sys.argv > 1 then
		for i = 1 to Array.length Sys.argv - 1 do
			print_tree (open_in Sys.argv.(i))
		done
	else
		print_tree stdin
