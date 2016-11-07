open Hashtbl

let ram : (int, int) Hashtbl.t = Hashtbl.create 100
let acc = ref 0

let op (op, addr1, addr2) =
    acc := op (find ram addr1) (find ram addr2)

let not addr =
    acc := 1-(find ram addr)

let jmpz e1 e2 =
    if(!acc == 0) then e2 else e1

let mv addr1 addr2 = replace ram addr2 (find ram addr1)

let st addr = replace ram addr !acc

let ldc n = acc := n

let ldr addr = acc := find ram addr
