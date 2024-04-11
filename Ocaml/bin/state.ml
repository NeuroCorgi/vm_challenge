type state = {
    memory    : Machine.Memory.memory;
    registers : Machine.Memory.registers;
    stack     : Machine.Memory.stack;
  }

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
