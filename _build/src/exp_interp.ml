open Syntax
open Instruction_set
open Exp_store

let convert_operator operator = match operator with
    | Plus -> (+)
    | Minus -> (-)
    | Times -> ( * )
    | Divide -> (/)
    | Modulus -> (mod)
    | And -> (fun a -> fun b -> b*a)
    | Or -> (fun a -> fun b -> b+a)
    | Less -> (fun a -> fun b -> if a<b then 1 else 0)
    | Leq -> (fun a -> fun b -> if a<=b then 1 else 0)
    | Greater -> (fun a -> fun b -> if a>b then 1 else 0)
    | Geq -> (fun a -> fun b -> if a>=b then 1 else 0)
    | Eq -> (fun a -> fun b -> if a==b then 1 else 0)
    | Noteq -> (fun a -> fun b -> if a<>b then 1 else 0)

let rec interp_exp symt expression =
    if(!stack_addr > stack_overflow) then
        failwith "Stackoverflow."
    else (match expression with
        | Operator(operator, e1, e2) ->
            let addr1 = interp_exp symt e1 in
            (match operator with
                 | And ->
                     if(Hashtbl.find ram addr1 == 0) then ldc 0
                     else op (convert_operator operator, addr1, interp_exp symt e2)
                 | Or ->
                    if(Hashtbl.find ram addr1 == 1) then ldc 1
                    else op (convert_operator operator, addr1, interp_exp symt e2)
                 | _ -> op (convert_operator operator, addr1, interp_exp symt e2));
            stack_addr := addr1;
            st addr1;
            addr1
        | Negate(e) ->
            let addr = interp_exp symt e in
            not addr;
            stack_addr := addr;
            st addr;
            addr
        | Identifier(s) -> lookup s symt
        | Deref(Identifier(s)) ->
            let addr = lookup s symt in
            if(addr < stack_overflow) then addr
            else
                let addr'= new_stack_addr() in
                mv addr addr';
                addr'
        | MyNull ->
            let addr = new_stack_addr() in
            ldc 0;
            st addr;
            addr
        | MyInteger(i) ->
            let addr = new_stack_addr() in
            ldc i;
            st addr;
            addr
        | MyBoolean(b) ->
            let addr = new_stack_addr() in
            if(b) then ldc 1 else ldc 0;
            st addr;
            addr
        | Seq(e1, e2) ->
            let addr1 = interp_exp symt e1 in
            interp_exp symt e2
        | Asg(e1, e2) ->
            let addr1 = interp_exp symt e1 in
            let addr2 = interp_exp symt e2 in
            mv addr2 addr1;
            addr1
        | New(s, e1, e2) ->
            let addr1 = interp_exp symt e1 in
            let addr' = new_heap_addr() in
            mv addr1 addr';
            let addr2 = interp_exp ((s, addr')::symt) e2 in
            addr2
        | Let(s, e1, e2) ->
            let addr1 = interp_exp symt e1 in
            let addr2 = interp_exp ((s, addr1)::symt) e2 in
            mv addr2 addr1;
            stack_addr := addr1;
            addr1
        | While(e1, e2) ->
            let addr1 = interp_exp symt e1 in
            stack_addr := addr1;
            let e = jmpz e2 Nothing in
            let addr = interp_exp symt e in
            if(addr <> -1) then interp_exp symt (While(e1, e2))
            else addr
        | For(s, e1, e2, e3) ->
            let addr1 = interp_exp symt e1 in
            let addr2 = interp_exp symt e2 in
            op (convert_operator Leq, addr1, addr2);
            stack_addr := addr1;
            let e = jmpz e3 Nothing in
            let addr = interp_exp ((s, addr1)::symt) e in
            let addr' = new_stack_addr() in
            ldc 1;
            st addr';
            op (convert_operator Plus, addr1, addr');
            st addr';
            if(addr <> -1) then interp_exp symt (For(s, MyInteger(Hashtbl.find ram addr'), e2, e3))
            else addr
        | If(e1, e2, e3) ->
            let addr1 = interp_exp symt e1 in
            stack_addr := addr1;
            ldr addr1;
            let e = jmpz e2 e3 in
            let addr = interp_exp symt e in
            addr
        | Read ->
            ldr read_addr;  (* register for reading *)
            let addr = new_stack_addr() in
            st addr;
            addr
        | Print(e) ->
            let addr = interp_exp symt e in
            mv addr print_addr; (* register for printing *)
            stack_addr := addr;
            Printf.printf "%d" (Hashtbl.find ram addr);
            print_addr
        | Nothing -> -1
        | _ -> failwith "Not implemented yet.")

let interpret program = match program with
    | [] -> 0
    | ("main", [], expression)::[] ->
        stack_addr := print_addr + 1;
        Hashtbl.add ram read_addr 3; (* register for reading; set to 3 for testing purposes*)
        let addr = interp_exp [] expression in
        if(addr <> -1) then (Hashtbl.find ram addr) else -1
    | _ -> failwith "Not implemented yet."