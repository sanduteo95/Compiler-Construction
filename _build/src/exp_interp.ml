open Syntax
open Instruction_set
open Exp_store
open Type_checking

let overflow = 5000

let (&&) a b = if (a+b==2) then 1 else 0
let (||) a b = if (a+b>=1) then 1 else 0

let convert_operator operator = match operator with
    | Plus -> (+)
    | Minus -> (-)
    | Times -> ( * )
    | Divide -> (/)
    | Modulus -> (mod)
    | And -> (&&)
    | Or -> (||)
    | _ -> failwith "Not possible."

let rec interp_exp symt expression =
    if(List.length symt > overflow) then
        failwith "Stackoverflow."
    else (match expression with
        | Operator(operator, e1, e2) ->
            let addr1 = interp_exp symt e1 in
            (match operator with
                | Plus | Minus | Times | Divide | Modulus ->
                    let addr2 = interp_exp symt e2 in
                    op (convert_operator operator, addr1, addr2)
                | And ->
                    if(Hashtbl.find ram addr1 == 0) then ldc 0
                    else
                        let addr2 = interp_exp symt e2 in
                        op (convert_operator operator, addr1, addr2)
                | Or ->
                    if(Hashtbl.find ram addr1 == 1) then ldc 1
                    else
                        let addr2 = interp_exp symt e2 in
                        op (convert_operator operator, addr1, addr2)
                | _ ->
                    let addr2 = interp_exp symt e2 in
                    ( match operator with
                        | Less -> slt addr1 addr2
                        | Leq -> sle addr1 addr2
                        | Greater -> sgt addr1 addr2
                        | Geq -> sge addr1 addr2
                        | Eq -> seq addr1 addr2
                        | Noteq -> sne addr1 addr2));
            addr_base := addr1;
            st addr1;
            addr1
        | Negate(e) ->
            let addr = interp_exp symt e in
            not addr;
            st addr;
            addr
        | Identifier(s) -> lookup s symt
        | Deref(Identifier(s)) ->
            let addr = lookup s symt in
            let addr'= new_addr() in
            mv addr addr';
            addr'
        | MyNull ->
            let addr = new_addr() in
            ldc 0;
            st addr;
            addr
        | MyInteger(i) ->
            let addr = new_addr() in
            ldc i;
            st addr;
            addr
        | MyBoolean(b) ->
            let addr = new_addr() in
            if(b) then ldc 1 else ldc 0;
            st addr;
            addr
        | Seq(e1, e2) ->
            let _ = interp_exp symt e1 in
            interp_exp symt e2
        | Asg(e1, e2) ->
            let addr1 = interp_exp symt e1 in
            let addr2 = interp_exp symt e2 in
            mv addr2 addr1;
            addr1
        | New(s, e1, e2) ->
            let addr1 = interp_exp symt e1 in
            let addr2 = interp_exp ((s, addr1)::symt) e2 in
            addr2
        | Let(s, e1, e2) ->
            let addr1 = interp_exp symt e1 in
            let addr2 = interp_exp ((s, addr1)::symt) e2 in
            mv addr2 addr1;
            addr_base := addr1;
            addr1
        | While(e1, e2) ->
            let addr1 = interp_exp symt e1 in
            let e = jmpz e2 Nothing in
            let addr = interp_exp symt e in
            if(addr <> -1) then interp_exp symt (While(e1, e2))
            else addr
        | For(s, e1, e2, e3) ->
            let addr1 = interp_exp symt e1 in
            let addr2 = interp_exp symt e2 in
            sle addr1 addr2;
            let e = jmpz e3 Nothing in
            let addr = interp_exp ((s, addr1)::symt) e in
            let addr' = new_addr() in
            ldc 1;
            st addr';
            op (convert_operator Plus, addr1, addr');
            st addr';
            if(addr <> -1) then interp_exp symt (For(s, MyInteger(Hashtbl.find ram addr'), e2, e3))
            else addr
        | If(e1, e2, e3) ->
            let addr1 = interp_exp symt e1 in
            let e = jmpz e2 e3 in
            let addr2 = interp_exp symt e in
            addr2
        | Read ->
            ldr read_addr;
            read_addr
        | Print(e) ->
            let addr = interp_exp symt e in
            mv addr print_addr; (* register for printing *)
            Printf.printf "%d" (Hashtbl.find ram addr);
            print_addr
        | Nothing -> -1
        | _ -> failwith "Not implemented yet.")

let interpret program = match program with
    | [] -> 0
    | ("main", [], expression)::[] ->
        addr_base := 0;
        Hashtbl.add ram read_addr 3; (* register for reading *)
        let addr = interp_exp [] expression in
        if(addr <> -1) then (Hashtbl.find ram addr) else -1
    | _ -> failwith "Not implemented yet."