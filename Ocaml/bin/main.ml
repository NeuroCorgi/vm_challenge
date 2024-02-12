type memory = int array
type registers = int array
type stack = int Stack.t

type state = {
    memory    : memory;
    registers : registers;
    stack     : stack;
  }

exception Machine_halt
exception Machine_empty_stack

let is_value addr = 0 <= addr && addr <= 32767
let is_register addr = 32768 <= addr && addr <= 32775
let register_of_int i = i - 32768

let read_mem state = function
  | i when is_value i -> state.memory.(i)
  | _ -> invalid_arg "memory address expected"

let read_reg state = function
  | i when is_register i ->
     let reg = register_of_int i in
     state.registers.(reg)
  | _ -> invalid_arg "register address expected"

let read_stack state = Stack.pop state.stack

let read state = function
  | i when is_value i -> i
  | i when is_register i ->
     read_reg state i
  | _ -> invalid_arg "invalid address"

let write_mem state = function
  | i when is_value i -> Array.set state.memory i
  | _ -> invalid_arg "memory address expected"

let write_reg state = function
  | i when is_register i ->
     let reg = register_of_int i in
     Array.set state.registers reg
  | _ -> invalid_arg "register address expected"

let write_stack state value = Stack.push value state.stack

let write state = function
  | i when is_value i -> write_mem state i
  | i when is_register i -> write_reg state i
  | _ -> invalid_arg "invalid address"

let halt _state _pc = raise Machine_halt

let set state pc =
  let dest = read_mem state (pc + 1) in
  let orig = read state (read_mem state (pc + 2)) in
  write state dest orig;
  pc + 3

let push state pc =
  let value = read state (read_mem state (pc + 1)) in
  write_stack state value;
  pc + 2

let pop state pc =
  let dest = read_mem state (pc + 1) in
  try
    let value = read_stack state in
    write state dest value;
    pc + 2
  with
    Stack.Empty -> raise Machine_empty_stack

let eq state pc =
  let dest = read_mem state (pc + 1) in
  let op1  = read state (read_mem state (pc + 2)) in
  let op2  = read state (read_mem state (pc + 3)) in
  let res  = if op1 = op2 then 1 else 0           in
  write state dest res;
  pc + 4

let gt state pc =
  let dest = read_mem state (pc + 1) in
  let op1  = read state (read_mem state (pc + 2)) in
  let op2  = read state (read_mem state (pc + 3)) in
  let res  = if op1 > op2 then 1 else 0           in
  write state dest res;
  pc + 4

let jmp state pc =
  read state (read_mem state (pc + 1))

let jt state pc =
  let cond = read state (read_mem state (pc + 1)) in
  if cond = 0 then
    pc + 3
  else
    jmp state (pc + 1)

let jf state pc =
  let cond = read state (read_mem state (pc + 1)) in
  if cond = 0 then
    jmp state (pc + 1)
  else
    pc + 3

let add state pc =
  let dest = read_mem state (pc + 1) in
  let op1  = read state (read_mem state (pc + 2)) in
  let op2  = read state (read_mem state (pc + 3)) in
  let res  = (op1 + op2) mod 32768                in
  write state dest res;
  pc + 4

let mult state pc =
  let dest = read_mem state (pc + 1) in
  let op1  = read state (read_mem state (pc + 2)) in
  let op2  = read state (read_mem state (pc + 3)) in
  let res  = (op1 * op2) mod 32768                in
  write state dest res;
  pc + 4

let mod' state pc =
  let dest = read_mem state (pc + 1) in
  let op1  = read state (read_mem state (pc + 2)) in
  let op2  = read state (read_mem state (pc + 3)) in
  let res  = op1 mod op2                          in
  write state dest res;
  pc + 4

let and' state pc =
  let dest = read_mem state (pc + 1) in
  let op1  = read state (read_mem state (pc + 2)) in
  let op2  = read state (read_mem state (pc + 3)) in
  let res  = Int.logand op1 op2                   in
  write state dest res;
  pc + 4

let or' state pc =
  let dest = read_mem state (pc + 1) in
  let op1  = read state (read_mem state (pc + 2)) in
  let op2  = read state (read_mem state (pc + 3)) in
  let res  = Int.logor op1 op2                    in
  write state dest res;
  pc + 4

let not' state pc =
  let mask = 0x7fff in
  let dest = read_mem state (pc + 1) in
  let op   = read state (read_mem state (pc + 2)) in
  let res  = (Int.logand mask (Int.lognot op))    in
  write state dest res;
  pc + 3

let rmem state pc =
  let dest  = read_mem state (pc + 1) in
  let addr  = read state (read_mem state (pc + 2)) in
  let value = read_mem state addr                  in
  write state dest value;
  pc + 3

let wmem state pc =
  let addr  = read state (read_mem state (pc + 1)) in
  let value = read state (read_mem state (pc + 2)) in
  write state addr value;
  pc + 3

let call state pc =
  write_stack state (pc + 2);
  jmp state pc

let ret state _pc =
  try
    let pc = read_stack state in
    pc
  with
    Stack.Empty -> raise Machine_halt

let out' state pc =
  let value = read state (read_mem state (pc + 1)) in
  let char  = char_of_int value in
  print_char char;
  pc + 2

let buffer = ref Seq.Nil
let read_input () =
  match !buffer with
  | Seq.Nil ->
     begin
       let input = (read_line ()) ^ "\n" in
       match String.to_seq input () with
       | Seq.Cons (char, rest) ->
          buffer := rest ();
          char
       | Seq.Nil -> failwith "unreachable"
     end
  | Seq.Cons (char, rest) ->
     buffer := rest ();
     char

let in' state pc =
  let dest = read_mem state (pc + 1) in
  let input = read_input () in
  let value = int_of_char input in
  write state dest value;
  pc + 2

let noop _state pc = pc + 1

let rec exec state pc =
  let command = match read_mem state pc with
    | 0 -> halt
    | 1 -> set
    | 2 -> push
    | 3 -> pop
    | 4 -> eq
    | 5 -> gt
    | 6 -> jmp
    | 7 -> jt
    | 8 -> jf
    | 9 -> add
    | 10 -> mult
    | 11 -> mod'
    | 12 -> and'
    | 13 -> or'
    | 14 -> not'
    | 15 -> rmem
    | 16 -> wmem
    | 17 -> call
    | 18 -> ret
    | 19 -> out'
    | 20 -> in'
    | 21 -> noop
    | instr ->
       Printf.eprintf "Warning: unknown instruction at %d: %d\n" pc instr;
       failwith (Printf.sprintf "unknown instruction at %d: %d" pc instr)
    | exception Invalid_argument _ ->
       Printf.eprintf "Error: command is not in memory: %d\n" pc;
       failwith (Printf.sprintf "out of memory read: %d" pc)
  in
  exec state (command state pc)

let input_value channel =
  try
    let low_byte  = input_byte channel in
    let high_byte = input_byte channel in
    Int.shift_left high_byte 8 + low_byte
  with
    End_of_file -> 0

let input_program channel =
  let init_address _index =
    input_value channel
  in
  Array.init 32768 init_address

let () =
  let prog_name = Sys.argv.(0) in
  try
    let file_name = Sys.argv.(1) in
    let channel   = open_in_bin file_name in
    let state = {
        memory = input_program channel;
        registers = [|0; 0; 0; 0; 0; 0; 0; 0|];
        stack = Stack.create ()
      } in
    try exec state 0 with
    | Machine_halt -> Printf.printf "Info: Machine halted!\n";
    | Machine_empty_stack -> Printf.eprintf "Error: Machine broke on empty state\n";
    | Invalid_argument s -> Printf.eprintf "Error: %s" s;
    close_in channel
  with
  | Invalid_argument _ -> Printf.eprintf "Usage: %s <filename>\n" prog_name
  | Sys_error s -> Printf.eprintf "Error: %s\n" s
