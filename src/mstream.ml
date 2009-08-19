exception End_of_stream

type 'a node =  { pos:int; mutable el:'a;
		 mutable next:'a node option}

type 'a t = { mutable cur:'a node option; saved:'a node Stack.t; gen: unit -> 'a option}

let next t = 
  match t.cur with
    | None -> raise End_of_stream
    | Some c -> 
	begin
	  match c.next with
	    | None -> 
		begin 
		  match t.gen() with
		    | Some r -> 
			let el = c.el in
			let nnode = {el=r; next=None; pos=c.pos+1} in
			  c.next <- Some nnode;
			  t.cur <- Some nnode;
			  el
		    | None -> let el = c.el in t.cur <- None; 
			el 
		end
	    | Some n -> let el = c.el in t.cur <- Some n; 
		el
	end

let push t =
  match t.cur with
      None -> raise End_of_stream
    | Some c ->  Stack.push c t.saved

let pop t = t.cur <- Some (Stack.pop t.saved)

let skip t n = 
  for i=0 to n-1 do
    next t;
  done

let top t = 
  match t.cur with
      Some a -> a.el 
    | None -> raise End_of_stream

let drop t =
  let _ = Stack.pop t.saved in ()

let spos t = 
  match t.cur with
    | Some a -> a.pos
    | None -> raise End_of_stream

 
let of_generator gen = 
    match gen() with
      | Some el ->
	  {gen=gen; cur=Some {el=el;next=None;pos=0};saved=Stack.create()}
      | None -> raise End_of_stream

let advance t p =
  let s = p - (spos t) in
    skip t s
(*type 'a result = Success of int * 'a | Fail *)

let string_generator f str = 
  let i = ref 0 in
  (fun () ->
     if !i < String.length str then
       let v = (String.get str !i) in
         i:=!i+1;
         Some(f v)
     else None)
;;

let of_string_func f str = of_generator (string_generator f str)
