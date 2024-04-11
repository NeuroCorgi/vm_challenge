open State

exception Interrupt

let handle_interrupt state pc =
  let rec loop () =
    print_string "> ";
    let input = read_line () in
    match String.split_on_char ' ' input with
    | [] -> loop ();
    | command :: args ->
       match command with
       | ":q" | ":quit" -> exit 0
       | ":c" | ":continue" -> ();
       | ":save" ->
          begin
            match args with
            | [_filepath] ->
               print_endline "Saving";
               loop ()
            | _ ->
               print_endline "Wrong arguments";
               loop ()
          end
       | ":s" | ":set" ->
          begin
            match args with
            | [address; value] ->
               let address = int_of_string address in
               let value   = int_of_string value in
               write state address value;
               loop ();
            | _ ->
               print_endline "Wrong arguments";
               loop ()
          end
       | _ ->
          Printf.printf "Unknown command: %s" command;
          loop ();
  in
  Printf.printf "Interrupted\n";
  loop ();
  pc

let rec exec state pc =
  let open Instructions in
  let command = match read_mem state pc with
    | i when i == Halt.opcode -> Halt.run
    | i when i == Set.opcode-> Set.run
    | i when i == Push.opcode -> Push.run
    | i when i == Pop.opcode -> Pop.run
    | i when i == Eq.opcode -> Eq.run
    | i when i == Gt.opcode -> Gt.run
    | i when i == Jmp.opcode -> Jmp.run
    | i when i == Jt.opcode -> Jt.run
    | i when i == Jf.opcode -> Jf.run
    | i when i == Add.opcode -> Add.run
    | i when i == Mul.opcode -> Mul.run
    | i when i == Mod.opcode -> Mod.run
    | i when i == And.opcode -> And.run
    | i when i == Or.opcode -> Or.run
    | i when i == Not.opcode -> Not.run
    | i when i == Rmem.opcode -> Rmem.run
    | i when i == Wmem.opcode -> Wmem.run
    | i when i == Call.opcode -> Call.run
    | i when i == Ret.opcode -> Ret.run
    | i when i == Out.opcode -> Out.run
    | i when i == In.opcode -> In.run
    | i when i == Noop.opcode -> Noop.run
    | instr ->
       Printf.eprintf "Warning: unknown instruction at %d: %d\n" pc instr;
       failwith (Printf.sprintf "unknown instruction at %d: %d" pc instr)
    | exception Invalid_argument _ ->
       Printf.eprintf "Error: command is not in memory: %d\n" pc;
       failwith (Printf.sprintf "out of memory read: %d" pc)
  in
  let pc =
    try command state pc
    with
      Interrupt -> handle_interrupt state pc
  in
  exec state pc

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
  Array.init Machine.Memory.memory_size init_address

let () =
  let prog_name = Sys.argv.(0) in
  try
    let file_name = Sys.argv.(1) in
    let channel   = open_in_bin file_name in
    let program = input_program channel in
    let state = {
        memory = Machine.Memory.init_memory ~from:program ();
        registers = Machine.Memory.init_registers ();
        stack = Machine.Memory.init_stack ()
      } in
    ignore @@ Sys.signal Sys.sigint (Sys.Signal_handle (fun _ -> raise Interrupt));
    try exec state 0 with
    | Machine.Machine_halt -> Printf.printf "Info: Machine halted!\n";
    | Machine.Memory.Machine_empty_stack -> Printf.eprintf "Error: Machine broke on empty state\n";
    | Invalid_argument s -> Printf.eprintf "Error: %s" s;
                            close_in channel
  with
  | Invalid_argument _ -> Printf.eprintf "Usage: %s <filename>\n" prog_name
  | Sys_error s -> Printf.eprintf "Error: %s\n" s
