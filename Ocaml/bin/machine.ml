module Memory = struct
  type memory = int array
  type registers = int array
  type stack = int Stack.t

  let memory_size = 32768

  let init_memory ?from:mem () =
    match mem with
    | Some(mem) -> mem
    | None -> Array.init memory_size (fun _ -> 0)
  
  let init_registers ?from:regs () =
    match regs with
    | Some(regs) -> regs
    | None -> [|0; 0; 0; 0; 0; 0; 0; 0|]
  
  let init_stack ?from:stack () =
    match stack with
    | Some(stack) -> stack
    | None -> Stack.create ()

  exception Machine_empty_stack
end

exception Machine_halt
