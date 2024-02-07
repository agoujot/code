open Graphics
open Sortlib
let rec sort t i =
	if Array.length t = 1
	then t
	else
		let mid = ((Array.length t)/2) in
		let m = Array.sub t 0 mid
		in let n = Array.sub t mid (Array.length t - mid)
		in let rec fuse a b j = 
			if a = [||]
			then b
			else 
			if b = [||] 
			then a
			else
				if a.(0) < b.(0)
				then (let x = Array.sub a 1 (Array.length a - 1) in draw j a.(0); drawa (j+1) x; drawa (j+Array.length a) b; Array.append [|a.(0)|] (fuse x b (j+1)))
				else (let x = Array.sub b 1 (Array.length b - 1) in draw j b.(0); drawa (j+1) a; drawa (j+Array.length a+1) x; Array.append [|b.(0)|] (fuse a x (j+1)))
		in fuse (sort m i) (sort n (i+mid)) i
let () = set_window_title "Merge sort"; ignore (sort t 0); off()
