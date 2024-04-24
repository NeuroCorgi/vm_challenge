type t = {
    pc : int;
    state : State.state;
  }

val dump : string -> t -> unit
val load : string -> t
