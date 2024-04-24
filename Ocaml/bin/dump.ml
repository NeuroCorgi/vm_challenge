type t = {
    pc : int;
    state : State.state;
  }

let output_binary_state channel (state : State.state) =
  let output_array channel array =
    output_binary_int channel (Array.length array);
    Array.iter (output_binary_int channel) array;
  in
  let output_registers channel registers =
    Array.iter (output_binary_int channel) registers
  in
  let output_stack channel stack =
    output_binary_int channel (Stack.length stack);
    Stack.iter (output_binary_int channel) stack
  in
  output_array channel state.memory;
  output_registers channel state.registers;
  output_stack channel state.stack;
  ()

let input_binary_state channel =
  let input_array channel =
    let size = input_binary_int channel in
    let array = Array.init size (fun _ -> input_binary_int channel) in
    if size != Machine.Memory.memory_size then failwith("Wrong memory size");
    array
  in
  let input_registers channel =
    Array.init 8 (fun _ -> input_binary_int channel)
  in
  let input_stack _channel =
    let size = input_binary_int channel in
    let buffer = Array.init size (fun _ -> input_binary_int channel) in
    buffer |> Array.to_list |> List.rev |> List.to_seq |> Stack.of_seq
  in
  let memory = input_array channel in
  let registers = input_registers channel in
  let stack = input_stack channel in
  ({
      memory = memory;
      registers = registers;
      stack = stack;
    } : State.state)

let dump filename dump =
  let channel = open_out_bin filename in
  output_binary_int channel dump.pc;
  output_binary_state channel dump.state;
  ()

let load filename =
  let channel = open_in_bin filename in
  let pc = input_binary_int channel in
  let state = input_binary_state channel in
  ({ pc = pc; state = state } : t)
