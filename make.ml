(** Utility to compile files without having to bother about manually checking dependancies or making documentation *)
(** List of packages provided with Stdlib *)
let known_libs = ["array"; "arg"; "arraylabels"; "atomic"; "bigarray"; "bool"; "buffer"; "bytes"; "byteslabels"; "callback"; "char"; "complex"; "stdlib"; "condition"; "domain"; "digest"; "effect"; "either"; "ephemeron"; "filename"; "float"; "format"; "fun"; "gc"; "hashtbl"; "in_channel"; "int"; "int32"; "int64"; "lazy"; "lexing"; "list"; "listlabels"; "map"; "marshal"; "morelabels"; "mutex"; "nativeint"; "oo"; "option"; "out_channel"; "parsing"; "printexc"; "printf"; "queue"; "random"; "result"; "scanf"; "seq"; "set"; "semaphore"; "stack"; "stdlabels"; "string"; "stringlabels"; "sys"; "type"; "uchar"; "unit"; "weak"; "stdlib"]
(** Returns the list of the files in current working directory *)
let dir() = Array.to_list (Sys.readdir (Sys.getcwd()))
(** The list of the arguments given on the command line *)
let arg = Array.to_list (Array.sub Sys.argv 1 (Array.length Sys.argv - 1))
(** Executes the unix command given *)
let cmd s = ignore (Unix.system s)
(** The regexp pattern for a module name *)
let modu = Str.regexp {|^[A-Z][a-z_0-9\.]*$|}
(** Applying {!modu} *)
let ismod s = Str.string_match modu s 0
(** Removes up leftovers from compilation from the working directory (takes a list of names, to be called with {!dir}()) *)
let rec clean =
	function
	| [] -> ()
	| h::t -> (if List.mem (Filename.extension h) [".cmi"; ".cmo"; ".cmx"; ".mli"; ".o"; ".html"] && Sys.file_exists h && not (ismod (Filename.remove_extension h)) then cmd ("rm "^h); clean t)
(** Removes duplicates from a list *)
let rec uni = 
	function
	| [] -> []
	| i::s -> let r = uni s in if List.mem i s then r else i::r
(** Filters module names from a list of words *)
let rec filtmod =
	function
	| [] -> []
	| w::r -> 
		(match w with 
		| "open" -> (match r with | [] -> [] | hd::tl -> if ismod hd then hd::filtmod tl else filtmod tl)
		| s when ismod s -> let li = String.split_on_char '.' s in if li = [s] then filtmod r else List.hd li :: filtmod r
		| _ -> filtmod r)
(** Removes special charcters (or characters in quotes) from a string *)
let rec empty s i st =
	if i = String.length s then "" else
	String.make 1 
	(match s.[i] with
	| 'A'..'Z' | 'a'..'z' | '0'..'9' | '.' | '_' when not st -> s.[i]
	| _ -> ' ')
	^empty s (i+1) (if s.[i] = '"' then not st else st)
(** Returns the list of third-party packages needed by the file given and the modules in currect directory that need to be used *)
let rec finddep n =
	let rec finddep' n =
		let f = n ^".ml" in
		let ic = open_in f in
		let rec read() = try let l = input_line ic in l::read() with End_of_file -> (close_in ic; []) in
		let words = 
		List.filter
		(fun s -> s <> "")
		(String.split_on_char ' ' (
		empty
		(String.concat " " (read()))
		0 false)) in
		let t = 
		(List.partition (fun s -> not (List.mem (String.lowercase_ascii s^".ml") (dir())))
		(List.filter 
		(fun s -> not (List.mem (String.lowercase_ascii s) known_libs))
		(List.map String.lowercase_ascii (uni(filtmod words))))) in
		let t_ = List.fold_left (fun l v -> let (a, b) = finddep' v in (fst l@a, snd l@b)) t (snd t) in
		if List.mem n (snd t_) then raise (Undefined_recursive_module ("make.ml", 64, 80)) else
		(uni (fst t_), uni (snd (t_)))
	in let (a, b) = finddep' n in
	(String.concat "," a, String.concat " " (List.map (fun s -> s ^ ".ml") (b@[n])))
(** Compiles the list of files given (files are in this module always given without their extension, .ml is assumed) *)
let rec comp l d s =
	match l with
	| [] -> ()
	| hd::tl ->
	(match hd with
	| "-d" -> comp tl (not d) ""
	| "-o" ->
		(match tl with
		| [] -> ()
		| na::rest -> comp rest d na)
	| _ ->
		let pkg, files = finddep hd in
		let out = if s = "" then String.capitalize_ascii hd else s in
		cmd ((if pkg = "" then "" else "ocamlfind ")^"ocamlopt -o "^out^(if pkg = "" then "" else " -package "^pkg^" -linkpkg")^" "^files);
		if d then cmd ((if pkg = "" then "" else "ocamlfind ")^"ocamldoc -html "^(if pkg = "" then "" else "-package "^pkg)^" "^files); 
		comp tl d "")
let () = comp arg true ""; cmd "cp styles.css style.css"; clean (dir())
