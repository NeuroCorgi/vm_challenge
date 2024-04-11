let write = print_char 

let buffer = ref Seq.Nil
let read () =
  match !buffer with
  | Seq.Nil ->
     begin
       let input = (read_line ()) ^ "\n" in
       match String.to_seq input () with
       | Seq.Cons (char, rest) ->
          buffer := rest ();
          char
       | Seq.Nil -> failwith "unreachable"
     end
  | Seq.Cons (char, rest) ->
     buffer := rest ();
     char
