module type Instruction = sig

  val opcode : int
  val run : State.state -> int -> int
  
end

module Halt : Instruction = struct
  let opcode = 0
  let run _state _pc = raise Machine.Machine_halt
end

module Set : Instruction = struct
  let opcode = 1
  let run state pc =
    let dest = State.read_mem state (pc + 1) in
    let orig = State.(read state (read_mem state (pc + 2))) in
    State.write state dest orig;
    pc + 3
end

module Push : Instruction = struct
  let opcode = 2
  let run state pc =
    let value = State.(read state (read_mem state (pc + 1))) in
    State.write_stack state value;
    pc + 2
end

module Pop : Instruction = struct
  let opcode = 3
  let run state pc =
    let dest = State.read_mem state (pc + 1) in
    try
      let value = State.read_stack state in
      State.write state dest value;
      pc + 2
    with
      Stack.Empty -> raise Machine.Memory.Machine_empty_stack
end

module Eq : Instruction = struct
  let opcode = 4
  let run state pc =
    let dest = State.read_mem state (pc + 1) in
    let op1  = State.(read state (read_mem state (pc + 2))) in
    let op2  = State.(read state (read_mem state (pc + 3))) in
    let res  = if op1 = op2 then 1 else 0           in
    State.write state dest res;
    pc + 4
end

module Gt : Instruction = struct
  let opcode = 5
  let run state pc =
    let dest = State.read_mem state (pc + 1) in
    let op1  = State.(read state (read_mem state (pc + 2))) in
    let op2  = State.(read state (read_mem state (pc + 3))) in
    let res  = if op1 > op2 then 1 else 0           in
    State.write state dest res;
    pc + 4
end

module Jmp : Instruction = struct
  let opcode = 6
  let run state pc =
    State.(read state (read_mem state (pc + 1)))
end

module Jt : Instruction = struct
  let opcode = 7
  let run state pc =
    let cond = State.(read state (read_mem state (pc + 1))) in
    if cond = 0 then
      pc + 3
    else
      Jmp.run state (pc + 1)
end

module Jf : Instruction = struct
  let opcode = 8
  let run state pc =
    let cond = State.(read state (read_mem state (pc + 1))) in
    if cond = 0 then
      Jmp.run state (pc + 1)
    else
      pc + 3
end

module Add : Instruction = struct
  let opcode = 9
  let run state pc =
    let dest = State.read_mem state (pc + 1) in
    let op1  = State.(read state (read_mem state (pc + 2))) in
    let op2  = State.(read state (read_mem state (pc + 3))) in
    let res  = (op1 + op2) mod 32768                in
    State.write state dest res;
    pc + 4
end

module Mul : Instruction = struct
  let opcode = 10
  let run state pc =
    let dest = State.read_mem state (pc + 1) in
    let op1  = State.(read state (read_mem state (pc + 2))) in
    let op2  = State.(read state (read_mem state (pc + 3))) in
    let res  = (op1 * op2) mod 32768                in
    State.write state dest res;
    pc + 4
end

module Mod : Instruction = struct
  let opcode = 11
  let run state pc =
    let dest = State.read_mem state (pc + 1) in
    let op1  = State.(read state (read_mem state (pc + 2))) in
    let op2  = State.(read state (read_mem state (pc + 3))) in
    let res  = op1 mod op2                          in
    State.write state dest res;
    pc + 4
end

module And : Instruction = struct
  let opcode = 12
  let run state pc =
    let dest = State.read_mem state (pc + 1) in
    let op1  = State.(read state (read_mem state (pc + 2))) in
    let op2  = State.(read state (read_mem state (pc + 3))) in
    let res  = Int.logand op1 op2                   in
    State.write state dest res;
    pc + 4
end

module Or : Instruction = struct
  let opcode = 13
  let run state pc =
    let dest = State.read_mem state (pc + 1) in
    let op1  = State.(read state (read_mem state (pc + 2))) in
    let op2  = State.(read state (read_mem state (pc + 3))) in
    let res  = Int.logor op1 op2                    in
    State.write state dest res;
    pc + 4
end

module Not : Instruction = struct
  let opcode = 14
  let run state pc =
    let mask = 0x7fff in
    let dest = State.read_mem state (pc + 1) in
    let op   = State.(read state (read_mem state (pc + 2))) in
    let res  = (Int.logand mask (Int.lognot op)) in
    State.write state dest res;
    pc + 3
end

module Rmem : Instruction = struct
  let opcode = 15
  let run state pc =
    let dest  = State.read_mem state (pc + 1) in
    let addr  = State.(read state (read_mem state (pc + 2))) in
    let value = State.read_mem state addr in
    State.write state dest value;
    pc + 3
end

module Wmem : Instruction = struct
  let opcode = 16
  let run state pc =
    let addr  = State.(read state (read_mem state (pc + 1))) in
    let value = State.(read state (read_mem state (pc + 2))) in
    State.write state addr value;
    pc + 3
end

module Call : Instruction = struct
  let opcode = 17
  let run state pc =
    State.write_stack state (pc + 2);
    Jmp.run state pc
end

module Ret : Instruction = struct
  let opcode = 18
  let run state _pc =
    try
      let pc = State.read_stack state in
      pc
    with
      Stack.Empty -> raise Machine.Machine_halt
end

module Out : Instruction = struct
  let opcode = 19
  let run state pc =
    let value = State.(read state (read_mem state (pc + 1))) in
    let char  = char_of_int value in
    Io.write char;
    pc + 2
end

module In : Instruction = struct
  let opcode = 20

  let run state pc =
    let dest = State.read_mem state (pc + 1) in
    let input = Io.read () in
    let value = int_of_char input in
    State.write state dest value;
    pc + 2
end

module Noop : Instruction = struct
  let opcode = 21
  let run _state pc = pc + 1
end

