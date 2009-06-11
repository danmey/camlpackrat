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

let of_generator gen = 
    match gen() with
      | Some el ->
	  {gen=gen; cur=Some {el=el;next=None;pos=0};saved=Stack.create()}
      | None -> raise End_of_stream
(*
let seek pos str =
  match str.cur with Nil -> raise End_of_stream 
    | Node a -> 
        let p = a.pos in
          if p > pos then
*)          
let string_generator str = 
  let i = ref 0 in
  (fun () ->
     if !i < String.length str then
       let v = Some (String.get str !i) in
         i:=!i+1;
         v
     else None)
;;

(*
type 'a instruction = 
    Push | Pop | Next | Skip of int | Get of ('a -> unit)

let run stream = function
  | Push::xs -> push stream
  | Pop::xs -> pop stream
  | Next::xs -> next stream
  | (Skip n)::xs -> skip stream n
  | (Get f)::xs -> f (top stream)
*)  
