(** Utility to compile files without having to bother about manually checking dependencies or making documentation *)
exception Cyclical_dependence of string list
exception Missing_package of string
(** Raised when there is an unfinished something at the end. *)
exception Unfinished of string
exception Missing_file of string
(** Help message *)
let info = 
"This is a utility to compile .ml files to native.
Syntax:
    build [options] filenames
Give it the name of the file, without the extension, and it will compile it.
The executable will have the module name corresponding to the file name (capitalized).
(This requires ocamlfind.)
Modules not from stdlib will be scanned for in current directory and its children.
It will also clean up (and optionally make html doc).
(What I mean by 'clean up' is 'destroy every file from compilation except the executable'.)

Options :
 -o [name] : change the name of the executable.
 -d : ensable documentation.
 -c : disable cleaning up.
 -h : shows this.
 -i [directory list] : add those directories (and children) to the search path.
 -r : do not search in any sub directories.
 -g : get the stack trace."
(** The list of the arguments given on the command line *)
let arg = Array.to_list (Array.sub Sys.argv 1 (Array.length Sys.argv - 1))
(** Executes the unix command given *)
let cmd s = ignore (Unix.system s)
(** For displaying error messages. *)
let tablength = 4
(** Removes duplicates from a list *)
let rec uni = 
	function
	| [] -> []
	| i::s -> let r = uni s in if List.mem i s then r else i::r
(** String to list of chars *)
let explode s = let rec it i = if i = String.length s then [] else s.[i]::it (i+1) in it 0
(** List of chars to string *)
let implode l = String.concat "" (List.map (fun c -> String.make 1 c) l)
(** Returns the list of the files in directory *)
let dir s = Array.to_list @@ Sys.readdir s
(** Returns the list of files available *)
let scan nas = 
	let rec scan_ l p =
		match l with
		| [] -> [] 
		| h::t when Sys.is_directory (p^h) -> (if List.mem "-r" arg then [] else (scan_ (dir (p^h)) (p^h^"/")))@scan_ t p
		| h::t -> (p^h)::scan_ t p
	in uni @@ List.concat @@ List.map (fun s -> scan_ (dir s) s) @@ nas
(** Gives a filename without its path *)
let remove_path s = List.hd @@ List.rev @@ String.split_on_char '/' @@ s
(** Gets the path out of a filename *)
let extract_path s = String.sub s 0 (String.length s - String.length (remove_path s))
(** Gives the path where a file can be found from its name *)
let find_path i f = 
	let rec it = function 
		| [] -> raise (Missing_file f)
		| h::t -> if remove_path h = f^".ml" then Filename.remove_extension h else it t
	in it (scan i)
(** regs i s checks if regexp number i matches s *)
let regs = let open Str in
	let id = {|[a-z_][a-zA-Z0-9_']*|} and
	mo = {|[A-Z][a-z_0-9A-Z']*|} in 
	let fd = " *"^id^{| *[:=] *.+ *|} in
	let a = 
	[|regexp ("^"^mo^{|\(\.|}^id^{|\)?$|}); 						(* 0 if a word looks like Module or Module.something *)
	  regexp ("^"^id^{|*\.ml: No such file or directory$|});		(* 1 if a Sys_error message is for missing file *)
	  regexp ("^ocamlfind: Package `"^id^"' not found$");			(* 2 if an ocamlfind error is for missing package *) 
   	  regexp {|^{\([a-z_]*\)|.*|\1}$|}; 							(* 3 quoted strings *)
	  regexp {|^File .*$|}; 										(* 4 indicator at the beginning of some error messages *)
	  regexp {|^Error:.*$|}; 										(* 5 line starting with Error *)
	  regexp {|[ 	]*\^[ 	]*|};											(* 6 pointer line *)
	  regexp {|^[0-9]+ error(s) encountered$|}; 					(* 7 thank you, i can count *)
	  regexp {|^[0-9]+ | .*$|};										(* 8 code snippet *)
	  regexp ({|^{\(|}^id^" with "^{|\)?|}^fd^{|\(;|}^fd^{|\)*}$|});(* 9 a record *)
	  regexp ("stdlib__"^mo^{|\..*|})								(* 10 a stdlib package file *)
	  |] in
	(fun i s -> string_match a.(i) s 0)
(** List of stdlib packages (should adapt to installation, though expect NixOS) *)
let stdlib = 
	List.map String.lowercase_ascii @@
	List.map (fun s -> String.sub s 8 (String.length s - 8)) @@
	uni @@
	List.map Filename.remove_extension @@
	List.filter (regs 10) @@ 
	dir "/run/current-system/sw/lib/ocaml"
(** Removes leftovers from compilation from the working directory (takes a list of names, to be called with {!scan}()) *)
let rec clean =
	function
	| [] -> if Sys.file_exists "__build_errors__.txt" then cmd "rm __build_errors__.txt" (** because sometimes clean can itself close badly, which will destroy some of them but not all, which might cause even more errors if not taken care of *)
	| h::t -> 
		if not (List.mem "-c" arg) then 
		(if List.mem (Filename.extension h) [".cmi"; ".cmo"; ".cmx"; ".mli"; ".o"] 
			&& Sys.file_exists h 
			&& not @@ regs 0 @@ Filename.remove_extension @@ remove_path h 
		then cmd ("rm '"^h^"'"); 
		clean t)
(** Filters module names from a list of words *)
let rec filtmod =
	function
	| [] -> []
	| w::r -> 
		(match w with 
		| "open" -> (match r with | [] -> [] | hd::tl -> if regs 0 hd then hd::filtmod tl else filtmod tl)
		| s when regs 0 s -> let li = String.split_on_char '.' s in if li = [s] then filtmod r else List.hd li :: filtmod r
	| _ -> filtmod r)
(** Removes special charcters (or characters inside strings, comments, characters, or quoted strings, but not records) from a string *)
let rec empty s =
	let rec it l state qs =
		match l with
		| [] -> if state <> 0 then raise (Unfinished (match state with | 1 -> "string" | 2 -> "character" | 3 -> "comment" | _ when state = 4 -> "brace" | _ -> "none")) else []
		| '\\'::_::t -> ' '::it t state qs
		| '"'::t when state = 0 ->  ' '::it t 1 ""
  		| '"'::t when state = 1 -> ' '::it t 0 ""
		| '\''::t when state = 0 -> ' '::it t 2 ""
  		| '\''::t when state = 2 -> ' '::it t 0 ""
		| '('::'*'::t when state = 0 -> ' '::it t 3 ""
		| '*'::')'::t when state = 3 -> ' '::it t 0 ""
		| '{'::t when state = 0 -> it t 4 "{"
		| '}'::t when state = 4 -> if regs 3 (qs^"}") then ' '::it t 0 "" else if regs 9 (qs^"}") then it ([' ']@explode (String.sub qs 1 (String.length qs - 1))@[' ']@t) 0 "" else it t 4 (qs^"}")
		| c::t when state = 4 -> it t 4 (qs^String.make 1 c)
		| 'A'..'Z'::t | 'a'..'z'::t | '0'..'9'::t | '.'::t | '_'::t when state=0 -> (List.hd l)::it t 0 ""
		| _::t -> ' '::it t state qs
	in implode (it (explode s) 0 "")
(** Returns the list of third-party packages needed by the file given and the other files that need to be used *)
let rec finddep n i =
	let rec finddep_ n p =
		if List.mem n p then raise (Cyclical_dependence (List.map remove_path p)) else
		let f = n ^ ".ml" in
		let ic = open_in f in
		let rec read() = try let l = input_line ic in l::read() with End_of_file -> (close_in ic; []) in
		let words = 
		List.filter (fun s -> s <> "") @@ String.split_on_char ' ' @@ empty @@ String.concat " " @@ read() in
		let t = 
		let tmp =
		words |> 
		filtmod |> 
		uni |> 
		List.map String.lowercase_ascii 
		|> List.filter (fun s -> not (List.mem s stdlib)) |> 
		List.partition (fun s -> not @@ List.mem s @@ List.map Filename.remove_extension @@ List.map remove_path @@ scan i) in
		(fst tmp, List.map (find_path i) @@ snd @@ tmp) in
		let t_ = List.fold_left (fun l v -> let (a, b) = finddep_ v (n::p) in (fst l@a, snd l@b)) t (snd t) in
		(uni (fst t_), uni (snd (t_)))
	in let (a, b) = finddep_ n [] in
	(String.concat "," a, String.concat " "  @@ List.map (fun s -> s ^ ".ml") @@ (b@[n]))
(** Compiles the list of files given (files are in this module always given without their extension, .ml is assumed) *)
let rec comp l s i = 
	try begin
	match l with
	| [] -> clean(scan i)
	| hd::tl ->
	(match hd with
	| "-o" ->
		(match tl with
		| [] -> clean(scan i)
		| na::rest -> comp rest na i)
	| "-d" | "-c" | "-r" | "-g" -> comp tl "" i
	| "-h" -> print_endline info
	| "-i" -> 
		(match tl with
		| [] -> clean (scan i)
		| di::rest -> comp rest s (i@String.split_on_char ',' di))
	| _ ->
		let pkg, files = finddep hd i in
		let dirs = 
			let rec getdir = 
				function
				| [] -> []
				| x::s -> let p = extract_path x in if p = "" then getdir s else p::getdir s in
			String.concat "," @@ uni @@ getdir @@ String.split_on_char ' ' @@ files in
		let out = if s = "" then String.capitalize_ascii hd else s in
		cmd ((if pkg = "" then "" else "ocamlfind ")^"ocamlopt "^(if List.mem "-g" arg then "-g " else "")^"-o "^out^" -I +unix,+str "^(if dirs = "" then "" else " -I "^dirs)^(if pkg = "" then "" else " -package "^pkg^" -linkpkg")^" "^files^" 2> __build_errors__.txt");
		if List.mem "-d" arg then 
		cmd ((if pkg = "" then "" else "ocamlfind ")^"ocamldoc -html "^(if dirs = "" then "" else " -I /run/current-system/sw/lib/ocaml/5.1.1/site-lib/graphics/,+unix,+str"^dirs)^(if pkg = "" then "" else " -package unix,str,"^pkg)^" "^files^" 2> __build_errors__.txt");
		let ic = open_in "__build_errors__.txt" in 
		let result = In_channel.input_lines ic in 
		close_in ic; 
		ignore(
		List.fold_left 
			(fun flag l -> 
			if regs 2 l then 
				raise 
				(Missing_package 
				(String.sub 
					(String.sub l 20 (String.length l - 20)) 
					0 
					(String.length l - 31))) else 
			if regs 7 l then "" else (
			if regs 4 l then 
				(print_endline 
					("\x1b[1m"^
					l^
					"\x1b[22m"); 
				"") else (
			if regs 6 l then 
				(print_endline 
					("\x1b[1;31m"
					^flag^(Str.global_replace (Str.regexp {|	|}) " " l)^
					"\x1b[22;0m"); 
				"") else (
			if regs 5 l then 
				(print_endline 
					("\x1b[1;31mError\x1b[22;0m"^
					String.sub l 5 
					(String.length l - 5)); 
				"") else
			if regs 8 l then 
				(let s = 
					let lst = 
					String.split_on_char '|' l in 
					print_string @@ List.hd @@ lst;
					print_string "| ";
					String.concat "|" @@ List.tl lst
				in
				let rec count i = 
					if s.[i] <> char_of_int 9 
					then i-1 else 
					count (i+1) in 
				let c = count 1 in
				print_string (String.make (c*tablength) ' ');
				print_endline (String.sub l (1+c+String.length l - String.length s) (+String.length s-1-c)); 
				String.make (c * (tablength - 1)) ' ') else (
			print_endline l; "")))))
		"" result);
		if result <> [] then print_endline "\x1b[34;1m.\x1b[0m"; (* to distinguish outputs from this from outputs from compiler. *)
		comp tl "" i)
	end with x -> (clean(scan i);
	match x with 
	| Cyclical_dependence l -> print_endline ("Cyclical dependence. \nThe following dependence loop was found : "^String.concat " > " l)
	| Sys_error y when regs 1 y -> print_endline ("File given does not exist."^y)
	| Missing_file y -> print_endline (y^" disappeared during compilation.\nProbably due to another program manipulating files at the same time.")
	| Missing_package s -> print_endline ("Missing package: "^s)
	| Unfinished y -> print_endline("There was an unfinished "^y^" at the end of the file."^if y = "character" then "\nIt might be due to your using apostrophes in identifiers.\nIt is allowed, but don't." else "")
	| _ -> print_endline ("Other error : "^Printexc.to_string x))
let () = comp arg "" ["./"]
