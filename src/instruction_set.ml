open Hashtbl

let read_addr = 0 (* Register for getting user input. *)
let print_addr = 1 (* Register for printing. *)

let stack_addr = ref (print_addr + 1) (* The base of the stack. *)
let stack_overflow = 1000 (* The limit of the stack.*)
let heap_addr = ref (stack_overflow) (* The beginning of the heap. *)

let label_addr = ref 0

let new_stack_addr() =
  	stack_addr := !stack_addr + 1;
  	!stack_addr

let new_heap_addr() =
     heap_addr := !heap_addr + 1;
     !heap_addr

let new_label() =
    label_addr := !label_addr + 1;
    !label_addr

let ram : (int, int) Hashtbl.t = Hashtbl.create 100
let text : (int, Syntax.expression) Hashtbl.t = Hashtbl.create 100
let acc = ref 0

let op (op, addr1, addr2) =
    acc := op (find ram addr1) (find ram addr2)

let not addr =
    acc := 1-(find ram addr)

let jmpz e1 e2 =
    if(!acc == 0) then e2 else e1

let mv addr1 addr2 =
    replace ram addr2 (find ram addr1)

let st addr = replace ram addr !acc

let ldc n = acc := n

let ldr addr = acc := find ram addr

let call addr = find text addr

let load addr expression = add text addr expression