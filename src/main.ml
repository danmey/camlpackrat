let _ =
  let g = Pegparser.peg_grammar in
  let a_file = (open_in "test.txt") in
  let str = String.create (in_channel_length a_file) in
    try
      let rec loop i =
	String.set str i (input_char a_file);
	loop (i+1) 
      in
	loop 0;
	Printf.printf "Error\n"
    with _ ->
      begin
	try
	  let result = Peg.parse_grammar g str in
	    result;
	    ()
	with Peg.Noparse a -> Printf.printf "Parse error at: %s\nNumber of iterations:%d\n" a !Peg.iterations
      end
      
      
