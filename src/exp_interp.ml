open Syntax
open Instruction_set

let addr_base = ref 0

let rec interp_exp symt = function
    | Operator(op, e1, e2) ->
        let addr1 = interpret symt e1 in
        let addr = interpret symt e2 in
        op (convert_operator op, addr1, addr2);
        addr_base := addr1;
        st addr1;
        addr1
    | Identifier(s) ->
        let addr = lookup x symt in
        let addr'= new_addr () in
        mv addr addr';
        addr'
    | MyInteger(i) ->
        let addr = new_addr () in
        ldc i;
        st addr
    | Let(s, e1, e2) ->
        let addr1 = interpret symt s1 in
        let addr2 = interpret ((x, addr1)::symt) e2 in
        mv addr2 addr1;
        addr_base := addr1;
        addr1

let interpret =
    addr_base := 0;
    let addr = interp_exp [] exp in
    addr
