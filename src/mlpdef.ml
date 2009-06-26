type 'a result = Success of int * 'a | Fail

let make_stream_generator f str = 
  let i = ref 0 in
  (fun () ->
     if !i < String.length str then
       let v = (String.get str !i) in
         i:=!i+1;
         Some(f v)
     else None)
;;

let parse f string arule =
  let gen = make_stream_generator f string in
    arule (Mstream.of_generator gen)
  
