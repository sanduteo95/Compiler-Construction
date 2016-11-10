open Hashtbl

let read_addr = 0 (** Register for getting user input. *)
let print_addr = 1 (** Register for printing. *)

let stack_addr = ref (print_addr + 1) (** The base of the stack. *)
let stack_overflow = 1000 (** The limit of the stack.*)
let heap_addr = ref (stack_overflow) (** The beginning of the heap. *)

let label_addr = ref 0 (** The index of a label. *)

(** Increments the stack address. *)
let new_stack_addr() =
  	stack_addr := !stack_addr + 1;
  	!stack_addr

(** Increments the heap address. *)
let new_heap_addr() =
     heap_addr := !heap_addr + 1;
     !heap_addr

(** Increments the lavel index. *)
let new_label() =
    label_addr := !label_addr + 1;
    !label_addr

(** The ram for the values. *)
let ram : (int, int) Hashtbl.t = Hashtbl.create 100

(** The text storage. *)
let text : (int, string * string list * Syntax.expression) Hashtbl.t = Hashtbl.create 100
(** The accumulator. *)
let acc = ref 0

(** The operator instruction. *)
let op (op, addr1, addr2) =
    acc := op (find ram addr1) (find ram addr2)

(** The not operator intstruction *)
let not addr =
    acc := 1-(find ram addr)

(** The instruction to move the contents of one address to another address. *)
let mv addr1 addr2 =
    replace ram addr2 (find ram addr1)

(** The instruction to store the contents of the accumulator into an address. *)
let st addr = replace ram addr !acc

(** The instruction to load a constant into the accumulator. *)
let ldc n = acc := n

(** The instruction to load the contents of an address into the accumulator. *)
let ldr addr = acc := find ram addr

(** The instruction to store the contents in the heap, pointed at by the stack, to a new addres on the stack. *)
let str addr1 addr2 =
    ldr addr1;
    let haddr = !acc in
    ldr haddr;
    st addr2

(** The instruction to move the contents in the heap, pointed at by the stack, to another address on the stack. *)
let mvr addr1 addr2 =
    ldr addr1;
    let haddr = !acc in
    mv addr2 haddr;
    stack_addr := addr1;
    ldr haddr;
    st addr2

(** The instruction to jump to an address if accumulator is 0. *)
let jmpz e1 e2 =
    if(!acc == 0) then e2 else e1

(** The instruction allocates space on the heap. *)
let alloc saddr haddr addr =
    ldc haddr;
    st saddr;
    mv addr haddr

let call addr =
    ldr addr;
    let haddr = !acc in
    find text haddr

let load saddr haddr (s, ps, expression) =
    add ram saddr haddr;
    add text haddr (s, ps, expression)