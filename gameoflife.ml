open Graphics
open CAT
let () = ini(); go
(fun l v -> let rec cou l = match l with | [] -> 0 | i::s -> if i = white then 1+cou s else cou s in let c = cou l in if c = 3 then white else if c = 2 then v else black)
[(1, 1);(1, 0);(1, -1);(0, 1);(0, -1);(-1, 1);(-1, 0);(-1, -1)]
100
[|white; black|]
(fun () -> let r = Random.int 2 in if r = 1 then white else black)
