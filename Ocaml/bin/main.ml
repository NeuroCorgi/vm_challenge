type memory = int array
type registers = int array
type stack = int Stack.t

type state = {
    memory    : memory;
    registers : registers;
    stack     : stack;
  }

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
    print_endline "Nothing here yet"
  with
  | Invalid_argument _ -> Printf.eprintf "Usage: %s <filename>\n" prog_name
  | Sys_error s -> Printf.eprintf "Error: %s\n" s
