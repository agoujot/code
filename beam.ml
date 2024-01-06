open Graphics
open CAT
let () = ini(); go
(fun l v -> let o = List.nth l 0::List.nth l 1::List.nth l 2::List.nth l 3::[] and
d = List.nth l 4::List.nth l 5::List.nth l 6::List.nth l 7::[] in
let test l_ col = List.exists (fun x -> x=col) l_ in 
if v = black then (if test o green && (not (test d red) || (test d blue && test o blue)) then green else black) else
if v = red then black else
if v = green then red else
if (not (test o blue) && not(test d green) && test o red) then green else blue)
[(1, 0);(-1, 0);(0, -1);(0, 1);(1, 1);(-1, 1);(1, -1);(-1, -1)]
100
[|black; blue; green; red|]
(fun () -> let r = Random.int 10 in if r < 5 then black else if r < 7 then red else if r < 9 then green else blue)
