exception Interrupt

let fired = ref false

let uninstall () =
  Sys.(signal sigint Signal_default)
  |> ignore

let signal_handler = 
  Sys.Signal_handle
    (fun _ ->
      uninstall ();
      raise Interrupt
    )

let install () =
  Sys.(signal sigint signal_handler)
  |> ignore


let handle (state : State.state) pc =
  Printf.printf "Interrupted\n";
  let pc = ref pc in
  let state = ref state in
  let running = ref true in
  while !running do
    print_string "> ";
    let input = read_line () in
    match String.split_on_char ' ' input with
    | [] -> ();
    | command :: args ->
       match command with
       | ":q" | ":quit" -> exit 0
       | ":c" | ":continue" -> running := false
       | ":load" ->
          begin
            match args with
            | [filename] ->
               print_endline "Loading";
               let dump = Dump.load filename in
               pc := dump.pc;
               state := dump.state
            | _ ->
               print_endline "Wrong arguments";
          end
       | ":save" ->
          begin
            match args with
            | [filename] ->
               print_endline "Saving";
               let dump : Dump.t = {
                   pc = !pc;
                   state = !state;
                 }
               in
               Dump.dump filename dump;
            | _ ->
               print_endline "Wrong arguments";
          end
       | ":s" | ":set" ->
          begin
            match args with
            | [address; value] ->
               let address = int_of_string address in
               let value   = int_of_string value in
               State.write !state address value;
            | _ ->
               print_endline "Wrong arguments";
          end
       | ":g" | ":get" ->
          begin
            match args with
            | [address] ->
               let address = int_of_string address in 
               let value = State.(read !state address) in
               print_int value;
               print_newline ()
            | _ ->
               print_endline "Wrong arguments";
          end
       | ":step" ->
          begin
            let exec n =
              for i = 0 to n do
                let open Instructions in
                let (module Instr : Instruction) =
                  try match_opcode (State.read_mem !state !pc) with
                    Unknown_instruction instr ->
                    (module struct
                       let opcode = instr
                       let run _state pc = pc + 1
                       let disassemble _memory i =
                         Printf.printf "%d\t%d\n" i instr;
                         i + 1
                     end : Instruction)
                in
                Instr.disassemble !state.memory !pc |> ignore;
                pc := Instr.run !state !pc
              done;
            in
            match args with
            | [] -> exec 1
            | [times] ->
               let times = int_of_string times in
               exec times
            | _ ->
               print_endline "Wront arguments";
          end
       | _ ->
          Printf.printf "Unknown command: %s" command;
  done;
  install ();
  (!pc, !state)
