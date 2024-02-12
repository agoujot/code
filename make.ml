(** Utility to compile files without having to bother about manually checking dependencies or making documentation *)
(** Raised when modules mutually depend on each other *)
exception Cyclical_dependance of string list
(** Raised when a module is missing *)
exception Missing_package of string
(** Help message *)
let info = 
"This is a utility to compile .ml files to native that automatically checks dependancies.
Give it the name of the file, without the extension, and it will compile it.
The executable will have the module name corresponding to the file name (capitalized).
If you want to change it, 
For example, if you do
./Make foobar
this will try to compile foobar.ml, including its dependencies (this uses ocamlfind), to produce the executable Foobar.
Modules not from stdlib will be scaned for in current directory and its children.
This will by default make html documentation with ocamldoc (disable with -d) and clean up all files produced by the compilation except the executable and the html (disable with -c).
-h shows this message."
(** List of packages automatically opened (depends on your installation, here in 4.13) *)
let stdlib = 
	["array"; "arg";       "arraylabels"; "atomic";   "bigarray";    "bool"; "buffer"; "bytes";  "byteslabels";  "callback"; "char";  "complex"; "stdlib"; "condition"; "domain"; "digest";  
	"either"; "ephemeron"; "filename";    "float";    "format";      "fun";  "gc";     "genlex"; "hashtbl";      "int";      "int32"; "int64";   "lazy";   "lexing";    "list";   "listlabels";  
	"map";    "marshal";   "morelabels";  "nativeint";"obj";         "oo";   "option"; "parsing";"pervasives";   "printexc"; "printf"; "queue";  "random"; "result";    "scanf";  "seq";  
	"set";    "stack";     "stdlabels";   "string";   "stringlabels";"sys";  "type";   "uchar";  "unit";         "weak";     "stdlib"]
(** Returns the list of the files in current working directory *)
let dir() = Array.to_list (Sys.readdir (Sys.getcwd()))
(** Removes duplicates from a list *)
let rec uni = 
	function
	| [] -> []
	| i::s -> let r = uni s in if List.mem i s then r else i::r
(** String to list of chars *)
let explode s = let rec it i = if i = String.length s then [] else s.[i]::it (i+1) in it 0
(** List of chars to string *)
let implode l = String.concat "" (List.map (fun c -> String.make 1 c) l)
(** Returns the list of files available *)
let mods() = 
	let rec scan l p = 
		match l with
		| [] -> [] 
		| h::t when Sys.is_directory (p^h) -> (scan (Array.to_list (Sys.readdir (p^h))) (p^h^"/"))@scan t p
		| h::t -> if Filename.extension h = ".ml" then (p^(Filename.remove_extension h))::scan t p else scan t p
	in uni(scan (dir()) "./")
(** Gives a filename without its path *)
let remove_path s = List.hd @@ List.rev @@ String.split_on_char '/' @@ s
(** Gives the path where a file can be found from its name *)
let find_path f = 
	let rec it = function 
		| [] -> raise Not_found 
		| h::t -> if remove_path h = f then h else it t 
	in it (mods())
(** The list of the arguments given on the command line *)
let arg = Array.to_list (Array.sub Sys.argv 1 (Array.length Sys.argv - 1))
(** Executes the unix command given *)
let cmd s = ignore (Unix.system s)
(** Array of functions to check some regexp patterns *)
let regs = let open Str in
	[|(fun s -> string_match (regexp {|^[A-Z][a-z_0-9\.]*$|}) s 0); (* if a word looks like Module or Module.something *)
	  (fun s -> string_match (regexp {|.ml: No such file of directory$|}) s 0); (* if a Sys_error message is for missing file *)
	  (fun s -> string_match (regexp {|^ocamlfind: Package `[a-z_0-9]*' not found$|}) s 0)|] (* if an ocamlfind error is for missing package *)
(** Removes up leftovers from compilation from the working directory (takes a list of names, to be called with {!dir}()) *)
let rec clean =
	function
	| [] -> cmd "rm __make_errors__.txt"
	| h::t -> 
		if not (List.mem "-c" arg) then 
		(if List.mem (Filename.extension h) [".cmi"; ".cmo"; ".cmx"; ".mli"; ".o"; ".html"] 
			&& Sys.file_exists h 
			&& not (regs.(0) (Filename.remove_extension h)) 
		then cmd ("rm "^h); 
		clean t)
(** Filters module names from a list of words *)
let rec filtmod =
	function
	| [] -> []
	| w::r -> 
		(match w with 
		| "open" -> (match r with | [] -> [] | hd::tl -> if regs.(0) hd then hd::filtmod tl else filtmod tl)
		| s when regs.(0) s -> let li = String.split_on_char '.' s in if li = [s] then filtmod r else List.hd li :: filtmod r
		| _ -> filtmod r)
(** Removes special charcters (or characters in quotes or comments) from a string *)
let rec empty s =
	let rec it l str com =
		match l with
		| [] -> []
		| '"'::t when not com ->  ' '::it t (not str) com
		| '('::'*'::t when not str && not com -> ' '::it t str (not com)
		| '*'::')'::t when not str && com -> ' '::it t str (not com)
		| 'A'..'Z'::t | 'a'..'z'::t | '0'..'9'::t | '.'::t | '_'::t when not str && not com -> List.hd l::it t str com
		| _::t -> ' '::it t str com
	in implode (it (explode s) false false)
(** Returns the list of third-party packages needed by the file given and the other files that need to be used *)
let rec finddep n =
	let rec finddep' n p =
		if List.mem n p then raise (Cyclical_dependance (List.rev (n::p))) else
		let f = n ^".ml" in
		let ic = open_in f in
		let rec read() = try let l = input_line ic in l::read() with End_of_file -> (close_in ic; []) in
		let words = 
		List.filter (fun s -> s <> "") @@ String.split_on_char ' ' @@ empty @@ String.concat " " @@ read() in
		let t = 
		let tmp =
		List.partition (fun s -> not (List.mem s (List.map remove_path (mods()))))
		@@ List.filter (fun s -> not (List.mem s stdlib))
		@@ List.map String.lowercase_ascii @@ uni @@ filtmod words in
		(fst tmp, List.map find_path @@ snd @@ tmp) in
		let t_ = List.fold_left (fun l v -> let (a, b) = finddep' v (n::p) in (fst l@a, snd l@b)) t (snd t) in
		(uni (fst t_), uni (snd (t_)))
	in let (a, b) = finddep' n [] in
	(String.concat "," a, String.concat " " (List.map (fun s -> s ^ ".ml") (b@[n])))
(** Compiles the list of files given (files are in this module always given without their extension, .ml is assumed) *)
let rec comp l s = 
	match l with
	| [] -> ()
	| hd::tl ->
	(match hd with
	| "-o" ->
		(match tl with
		| [] -> ()
		| na::rest -> comp rest na)
	| "-d" | "-c" -> comp tl ""
	| "-h" -> print_endline info
	| _ ->
		let pkg, files = finddep hd in
		let out = if s = "" then String.capitalize_ascii hd else s in
		cmd ((if pkg = "" then "" else "ocamlfind ")^"ocamlopt -o "^out^(if pkg = "" then "" else " -package "^pkg^" -linkpkg")^" "^files^" 2> __make_errors__.txt"); 
		if not (List.mem "-d" arg) then 
		cmd ((if pkg = "" then "" else "ocamlfind ")^"ocamldoc -html "^(if pkg = "" then "" else "-package "^pkg)^" "^files^" 2> __make_errors__.txt");
		let ic = open_in "__make_errors__.txt" in 
		let result = In_channel.input_lines ic in 
		close_in ic; 
		List.iter 
		(fun s -> if regs.(2) s then raise (Missing_package (String.sub (String.sub s 20 (String.length s - 20)) 0 (String.length s - 31))) else print_endline s)
		result;
		comp tl "")
let () = 
	try 
	(comp arg ""; clean (dir())) 
	with x -> (clean (dir()); 
	match x with 
	| Cyclical_dependance l -> print_endline ("Cyclical dependence. \nThe following dependence loop was found : "^String.concat " > " l)
	| Sys_error y when regs.(1) y -> print_endline "File given does not exist."
	| Not_found -> print_endline "A file disappeared during compilation."
	| Missing_package s -> print_endline ("Missing package: "^s)
	| _ -> print_endline "Unforeseen error (maybe due to input) : "; raise x)
