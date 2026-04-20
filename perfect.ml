open Graphics
let rec it n =	
	set_color black; fill_rect 0 0 1000 1000; 
	let z = 2. *. Float.pi /. float_of_int n (* radians between each node *) in
	let co = Array.mapi (fun i n -> (int_of_float ((1.+.cos(float_of_int i *. z))*.500.), int_of_float ((1.+.sin(float_of_int i *. z))*.500.))) (Array.make n 1000) in
	let link i j = 
		let a, b = co.(i) and c, d = co.(j) in
		moveto a b;
		lineto c d in
	let rec di i j =
		if i >= n then () else (
		if j >= i then di (i+1) 0 else
		(link i j;
		di i (j+1))) in
	di 0 0;
	synchronize();
	ignore(read_key());
	it (n+1)
let () =
	auto_synchronize false;
	open_graph "";
	resize_window 1000 1000; 
	set_color white;
	it 1
