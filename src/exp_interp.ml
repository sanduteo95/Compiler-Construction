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
        | Identifier(s) ->
            let addr = lookup s symt in
            let addr'= new_stack_addr() in
            mv addr addr';
            addr'
        | Deref(e) ->
            let addr = interp_exp symt e in
            ldr addr;
            let addr' = !acc in
            let addr'' = new_stack_addr() in
            ldr addr';
            st addr'';
            addr''
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
            ldr addr1;
            let haddr = !acc in
            let addr2 = interp_exp symt e2 in
            mv addr2 haddr;
            stack_addr := addr1;
            ldc haddr;
            st addr2;
            addr2
        | New(s, e1, e2) ->
            let addr1 = interp_exp symt e1 in
            let saddr = new_stack_addr() in
            let haddr = new_heap_addr() in
            ldc haddr;
            st saddr;
            mv addr1 haddr;
            let addr2 = interp_exp ((s, saddr)::symt) e2 in
            addr2
        | Let(s, e1, e2) ->
            let addr1 = interp_exp symt e1 in
            let addr2 = interp_exp ((s, addr1)::symt) e2 in
            (if addr2 != addr1 then mv addr2 addr1);
            stack_addr := addr1;
            addr1
        | While(e1, e2) ->
            let initial_addr = !stack_addr in
            let addr1 = interp_exp symt e1 in
            let e = jmpz e2 Nothing in
            let addr = interp_exp symt e in
            stack_addr := initial_addr;
            if(addr <> -1) then interp_exp symt (While(e1, e2))
            else addr
        | For(s, e1, e2, e3) ->
            let initial_addr = !stack_addr in
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
            stack_addr := initial_addr;
            if(addr <> -1) then interp_exp symt (For(s, MyInteger(Hashtbl.find ram addr'), e2, e3))
            else addr
        | If(e1, e2, e3) ->
            let addr1 = interp_exp symt e1 in
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
            print_addr
        (* | Application(exp, ps) ->
            let initial_addr = !stack_addr;
            let addrs = List.map(interp_exp symt) ps in
            List.map (fun addr -> let new_addr = new_stack_addr() in ldc addr; st new_addr) addrs;
            let new_addr = new_stack_addr() in
            ld initial_addr;
            st new_addr;
            let addr = lookup s symt in
            let addr' = interp_exp symt (call addr);
            ldc addr'; *)

        | Nothing -> -1
        | _ -> failwith "Not implemented yet.")

let interpret program = match program with
    | [] -> 0
    | ("main", [], expression)::[] ->
        stack_addr := print_addr + 1;
        Hashtbl.add ram read_addr 3; (* register for reading; set to 3 for testing purposes*)
        let addr = interp_exp [] expression in
        if(addr <> -1 && addr <> 2) then (Hashtbl.find ram addr) else addr
    (* | (s, ps, expression)::program ->
        let saddr = new_stack_addr() in
        load saddr expression;
        interpret program *)