type memory = int array
type registers = int array
(* type stack = int Stack.t *)

type state = {
    memory    : memory;
    registers : registers;
    (* stack     : stack; *)
  }

exception Machine_halt

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

(* let read_stack state = Stack.pop state.stack *)

let read state = function
  | i when is_value i -> i
  | i when is_register i ->
     read_reg state i
  | _ -> invalid_arg "invalid address"

let halt _state _pc = raise Machine_halt

let out' state pc =
  let value = read state (read_mem state (pc + 1)) in
  let char  = char_of_int value in
  print_char char;
  pc + 2

let noop _state pc = pc + 1

let rec exec state pc =
  let command = match read_mem state pc with
    | 0 -> halt
    | 19 -> out'
    | 21 -> noop
    | instr ->
       Printf.eprintf "Warning: unknown instruction at %d: %d\n" pc instr;
       noop
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
        (* stack = Stack.create () *)
      } in
    try exec state 0 with
    | Machine_halt -> Printf.printf "Info: Machine halted!\n";
    close_in channel
  with
  | Invalid_argument _ -> Printf.eprintf "Usage: %s <filename>\n" prog_name
  | Sys_error s -> Printf.eprintf "Error: %s\n" s
