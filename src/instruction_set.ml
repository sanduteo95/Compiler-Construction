open Hashtbl
let ram = Hashtbl.create 100
let acc = ref 0

let string_of_operator = function
    | ( + ) | ( +. ) -> "add"
    | ( - ) | ( -. ) -> "sub"
    | ( * ) | ( *. ) -> "mul"
    | ( / ) | ( /. ) -> "div"
    | ( && ) -> "and"
    | ( || ) -> "or"
    | _ -> "still need a few"


let op (op, addr1, addr2) =
    acc := op (find ram addr1) (find ram addr2)

let st addr = replace ram addr !acc

let ldc n = acc := n