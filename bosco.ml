open Graphics
open CAT
let () = ini(); go
(fun l v -> let rec cou l = match l with | [] -> 0 | i::s -> if i = white then 1+cou s else cou s in let c = cou l in if v = white then (if c >= 33 && c <= 57 then white else black) else if c >= 34 && c <= 45 then white else black)
(let rec b i j = if i = 6 then [] else if j = 6 then b (i+1) (-5) else if i = 0 && j = 0 then b i (j+1) else (i, j)::b i (j+1) in b (-5) (-5))
100
[|black; white|]
(fun () -> let r = Random.int 2 in if r = 1 then white else black)
