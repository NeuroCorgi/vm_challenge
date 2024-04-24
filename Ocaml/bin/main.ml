let rec exec state pc =
  let open Instructions in
  let (module Instr : Instructions.Instruction) =
    try match_opcode (State.read_mem state pc) with
    | Unknown_instruction instr ->
       Printf.eprintf "Warning: unknown instruction at %d: %d\n" pc instr;
       failwith (Printf.sprintf "unknown instruction at %d: %d" pc instr)
    | Invalid_argument _ ->
       Printf.eprintf "Error: command is not in memory: %d\n" pc;
       failwith (Printf.sprintf "out of memory read: %d" pc)
  in
  let (pc, state) =
    (* Possible synchronization issues *)
    try (Instr.run state pc, state)
    with
      Interrupt.Interrupt -> Interrupt.handle state pc
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

let run filename =
  try
    let channel = open_in_bin filename in
    let program = input_program channel in
    let state : State.state = {
        memory = Machine.Memory.init_memory ~from:program ();
        registers = Machine.Memory.init_registers ();
        stack = Machine.Memory.init_stack ()
      } in
    Interrupt.install ();
    try exec state 0 with
    | Machine.Machine_halt -> Printf.printf "Info: Machine halted!\n";
    | Machine.Memory.Machine_empty_stack -> Printf.eprintf "Error: Machine broke on empty state\n";
    | Invalid_argument s -> Printf.eprintf "Error: %s" s;
                            close_in channel
  with
  | Sys_error s -> Printf.eprintf "Error: %s\n" s

let disassemble dumpname =
  let dump = Dump.load dumpname in
  Printf.printf "\nPc: %d\nRegisters: %d %d %d %d %d %d %d %d\n\nMemory:\n"
    dump.pc
    dump.state.registers.(0)
    dump.state.registers.(1)
    dump.state.registers.(2)
    dump.state.registers.(3)
    dump.state.registers.(4)
    dump.state.registers.(5)
    dump.state.registers.(6)
    dump.state.registers.(7);
  let memory = dump.state.memory in
  let rec loop i =
    let open Instructions in
    if i == Machine.Memory.memory_size then exit 0;
    let next =
      begin
        let (module Instr : Instruction) =
          try match_opcode memory.(i)
          with
            Unknown_instruction instr ->
            (module struct
               let opcode = instr
               let run _state pc = pc
               let disassemble _memory i =
                 Printf.printf "%d\t%d\n" i instr;
                 i + 1
             end : Instruction)
        in
        Instr.disassemble memory i
      end
    in
    loop next
    in
    loop 0

let () =
  let prog_name = Sys.argv.(0) in
  let command = Sys.argv.(1) in
  let filename =
    try Sys.argv.(2)
    with
    | Invalid_argument _ -> Printf.eprintf "Usage: %s (run <filename> | disas <filename>)\n" prog_name; exit 1
  in
    
    match command with
    | "run" ->
       run filename
    | "disas" ->
       disassemble filename
    | _ -> ()
  (* | Sys_error s -> Printf.eprintf "Error: %s\n" s *)
