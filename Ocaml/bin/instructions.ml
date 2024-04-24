module type Instruction = sig

  val opcode : int
  val run : State.state -> int -> int
  val disassemble : int array -> int -> int
  
end

exception Unknown_instruction of int

module Halt : Instruction = struct
  let opcode = 0
  let run _state _pc = raise Machine.Machine_halt

  let disassemble _mem index =
    Printf.printf "%d\thalt\n" index;
    index + 1
end

module Set : Instruction = struct
  let opcode = 1
  let run state pc =
    let dest = State.read_mem state (pc + 1) in
    let orig = State.(read state (read_mem state (pc + 2))) in
    State.write state dest orig;
    pc + 3

  let disassemble memory i =
    Printf.printf "%d\tset\t%d\t%d\n" i memory.(i + 1) memory.(i + 2);
    i + 3
end

module Push : Instruction = struct
  let opcode = 2
  let run state pc =
    let value = State.(read state (read_mem state (pc + 1))) in
    State.write_stack state value;
    pc + 2

  let disassemble memory i =
    Printf.printf "%d\tpush\t%d\n" i memory.(i + 1);
    i + 2
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

  let disassemble memory i =
    Printf.printf "%d\tpop\t%d\n" i memory.(i + 1);
    i + 2
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

  let disassemble memory i =
    Printf.printf "%d\teq\t%d\t%d\t%d\n" i memory.(i + 1) memory.(i + 2) memory.(i + 3);
    i + 4
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

  let disassemble memory i =
    Printf.printf "%d\tgt\t%d\t%d\t%d\n" i memory.(i + 1) memory.(i + 2) memory.(i + 3);
    i + 4
end

module Jmp : Instruction = struct
  let opcode = 6
  let run state pc =
    State.(read state (read_mem state (pc + 1)))

  let disassemble memory i =
    Printf.printf "%d\tjmp\t%d\n" i memory.(i + 1);
    i + 2
end

module Jt : Instruction = struct
  let opcode = 7
  let run state pc =
    let cond = State.(read state (read_mem state (pc + 1))) in
    if cond = 0 then
      pc + 3
    else
      Jmp.run state (pc + 1)

  let disassemble memory i =
    Printf.printf "%d\tjt\t%d\t%d\n" i memory.(i + 1) memory.(i + 2);
    i + 3
end

module Jf : Instruction = struct
  let opcode = 8
  let run state pc =
    let cond = State.(read state (read_mem state (pc + 1))) in
    if cond = 0 then
      Jmp.run state (pc + 1)
    else
      pc + 3

  let disassemble memory i =
    Printf.printf "%d\tjf\t%d\t%d\n" i memory.(i + 1) memory.(i + 2);
    i + 3
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

  let disassemble memory i =
    Printf.printf "%d\tadd\t%d\t%d\t%d\n" i memory.(i + 1) memory.(i + 2) memory.(i + 3);
    i + 4
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

  let disassemble memory i =
    Printf.printf "%d\tmul\t%d\t%d\t%d\n" i memory.(i + 1) memory.(i + 2) memory.(i + 3);
         i + 4
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

  let disassemble memory i =
    Printf.printf "%d\tmod\t%d\t%d\t%d\n" i memory.(i + 1) memory.(i + 2) memory.(i + 3);
    i + 4
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

  let disassemble memory i =
    Printf.printf "%d\tand\t%d\t%d\t%d\n" i memory.(i + 1) memory.(i + 2) memory.(i + 3);
    i + 4
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

  let disassemble memory i =
    Printf.printf "%d\tor\t%d\t%d\t%d\n" i memory.(i + 1) memory.(i + 2) memory.(i + 3);
    i + 4
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

  let disassemble memory i =
    Printf.printf "%d\tnot\t%d\t%d\n" i memory.(i + 1) memory.(i + 2);
    i + 3
end

module Rmem : Instruction = struct
  let opcode = 15
  let run state pc =
    let dest  = State.read_mem state (pc + 1) in
    let addr  = State.(read state (read_mem state (pc + 2))) in
    let value = State.read_mem state addr in
    State.write state dest value;
    pc + 3

  let disassemble memory i =
    Printf.printf "%d\trmem\t%d\t%d\n" i memory.(i + 1) memory.(i + 2);
    i + 3
end

module Wmem : Instruction = struct
  let opcode = 16
  let run state pc =
    let addr  = State.(read state (read_mem state (pc + 1))) in
    let value = State.(read state (read_mem state (pc + 2))) in
    State.write state addr value;
    pc + 3

  let disassemble memory i =
    Printf.printf "%d\twmem\t%d\t%d\n" i memory.(i + 1) memory.(i + 2);
    i + 3
end

module Call : Instruction = struct
  let opcode = 17
  let run state pc =
    State.write_stack state (pc + 2);
    Jmp.run state pc

  let disassemble memory i =
    Printf.printf "%d\tjmp\t%d\n" i memory.(i + 1);
    i + 2
end

module Ret : Instruction = struct
  let opcode = 18
  let run state _pc =
    try
      let pc = State.read_stack state in
      pc
    with
      Stack.Empty -> raise Machine.Machine_halt

  let disassemble _memory i =
    Printf.printf "%d\tret\n" i;
    i + 1
end

module Out : Instruction = struct
  let opcode = 19
  let run state pc =
    let value = State.(read state (read_mem state (pc + 1))) in
    let char  = char_of_int value in
    Io.write char;
    pc + 2

  let disassemble memory i =
    (try
      let char = char_of_int memory.(i + 1) in
      Printf.printf "%d\tout\t%c\n" i char;
    with
      Invalid_argument _ ->
      Printf.printf "%d\tout\t%d" i memory.(i + 1));
    i + 2
end

module In : Instruction = struct
  let opcode = 20

  let run state pc =
    let dest = State.read_mem state (pc + 1) in
    let input = Io.read () in
    let value = int_of_char input in
    State.write state dest value;
    pc + 2

  let disassemble memory i =
    Printf.printf "%d\tin\t%d\n" i memory.(i + 1);
    i + 2
end

module Noop : Instruction = struct
  let opcode = 21
  let run _state pc = pc + 1

  let disassemble _memory i =
    Printf.printf "%d\tnoop\n" i;
    i + 1
end

let match_opcode = function
  | i when i == Halt.opcode -> (module Halt : Instruction)
  | i when i == Set.opcode-> (module Set : Instruction)
  | i when i == Push.opcode -> (module Push : Instruction)
  | i when i == Pop.opcode -> (module Pop : Instruction)
  | i when i == Eq.opcode -> (module Eq : Instruction)
  | i when i == Gt.opcode -> (module Gt : Instruction)
  | i when i == Jmp.opcode -> (module Jmp : Instruction)
  | i when i == Jt.opcode -> (module Jt : Instruction)
  | i when i == Jf.opcode -> (module Jf : Instruction)
  | i when i == Add.opcode -> (module Add : Instruction)
  | i when i == Mul.opcode -> (module Mul : Instruction)
  | i when i == Mod.opcode -> (module Mod : Instruction)
  | i when i == And.opcode -> (module And : Instruction)
  | i when i == Or.opcode -> (module Or : Instruction)
  | i when i == Not.opcode -> (module Not : Instruction)
  | i when i == Rmem.opcode -> (module Rmem : Instruction)
  | i when i == Wmem.opcode -> (module Wmem : Instruction)
  | i when i == Call.opcode -> (module Call : Instruction)
  | i when i == Ret.opcode -> (module Ret : Instruction)
  | i when i == Out.opcode -> (module Out : Instruction)
  | i when i == In.opcode -> (module In : Instruction)
  | i when i == Noop.opcode -> (module Noop : Instruction)
  | i -> raise (Unknown_instruction i)
