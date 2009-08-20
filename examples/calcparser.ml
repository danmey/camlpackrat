

type memo = {r_base:char;
  mutable number :int Mlpeg.result option;
  mutable plus :string Mlpeg.result option;
  mutable minus :string Mlpeg.result option;
  mutable times :string Mlpeg.result option;
  mutable divide :string Mlpeg.result option;
  mutable lb :string Mlpeg.result option;
  mutable rb :string Mlpeg.result option;
  mutable expr :int Mlpeg.result option;
  mutable sum :int Mlpeg.result option;
  mutable product :int Mlpeg.result option;
  mutable value :int Mlpeg.result option;
  mutable a_main :int Mlpeg.result option;
}

let __set_number memo v = memo.number <- v 
let __get_number memo = memo.number
let __set_plus memo v = memo.plus <- v 
let __get_plus memo = memo.plus
let __set_minus memo v = memo.minus <- v 
let __get_minus memo = memo.minus
let __set_times memo v = memo.times <- v 
let __get_times memo = memo.times
let __set_divide memo v = memo.divide <- v 
let __get_divide memo = memo.divide
let __set_lb memo v = memo.lb <- v 
let __get_lb memo = memo.lb
let __set_rb memo v = memo.rb <- v 
let __get_rb memo = memo.rb
let __set_expr memo v = memo.expr <- v 
let __get_expr memo = memo.expr
let __set_sum memo v = memo.sum <- v 
let __get_sum memo = memo.sum
let __set_product memo v = memo.product <- v 
let __get_product memo = memo.product
let __set_value memo v = memo.value <- v 
let __get_value memo = memo.value
let __set_a_main memo v = memo.a_main <- v 
let __get_a_main memo = memo.a_main

let rec memo_table_init ch = {
  r_base=ch;
  number= None;
  plus= None;
  minus= None;
  times= None;
  divide= None;
  lb= None;
  rb= None;
  expr= None;
  sum= None;
  product= None;
  value= None;
  a_main= None;
}

and number stream = 
  begin
    Mstream.push stream;
    let f = 
    (Mlpeg.string_of_char (Mstream.next stream).r_base)
     in
    begin
    if f.[0] >= '0' && f.[0] <= '9' then
      begin
        Mstream.push stream;
        let res = ref [] in
        let flag = ref true in
        while !flag do
        let loc = 
        (Mlpeg.string_of_char (Mstream.next stream).r_base)
         in
        begin
        if loc.[0] >= '0' && loc.[0] <= '9' then
          begin
            Mstream.drop stream;
            Mstream.push stream;
            res:=!res @ [loc]
          end
           else 
          begin
            Mstream.pop stream;
            flag := false
          end
        end
        done;
        let n = !res in
        let loc = 
         int_of_string (String.concat "" (f::n)) 
         in
        begin
        begin
          Mstream.drop stream;
          Mlpeg.Success ((Mstream.spos stream), loc)
        end
        end
      end
       else 
      begin
        Mstream.pop stream;
        Mlpeg.Fail
      end
    end
  end

and plus stream = 
  begin
    Mstream.push stream;
    let loc = 
    "+"
     in
    begin
    if (Mstream.next stream).r_base = '+' then
    begin
      Mstream.drop stream;
      Mlpeg.Success ((Mstream.spos stream), loc)
    end
    else
    begin
      Mstream.pop stream;
      Mlpeg.Fail
    end
    end
  end

and minus stream = 
  begin
    Mstream.push stream;
    let loc = 
    "-"
     in
    begin
    if (Mstream.next stream).r_base = '-' then
    begin
      Mstream.drop stream;
      Mlpeg.Success ((Mstream.spos stream), loc)
    end
    else
    begin
      Mstream.pop stream;
      Mlpeg.Fail
    end
    end
  end

and times stream = 
  begin
    Mstream.push stream;
    let loc = 
    "*"
     in
    begin
    if (Mstream.next stream).r_base = '*' then
    begin
      Mstream.drop stream;
      Mlpeg.Success ((Mstream.spos stream), loc)
    end
    else
    begin
      Mstream.pop stream;
      Mlpeg.Fail
    end
    end
  end

and divide stream = 
  begin
    Mstream.push stream;
    let loc = 
    "/"
     in
    begin
    if (Mstream.next stream).r_base = '/' then
    begin
      Mstream.drop stream;
      Mlpeg.Success ((Mstream.spos stream), loc)
    end
    else
    begin
      Mstream.pop stream;
      Mlpeg.Fail
    end
    end
  end

and lb stream = 
  begin
    Mstream.push stream;
    let loc = 
    "("
     in
    begin
    if (Mstream.next stream).r_base = '(' then
    begin
      Mstream.drop stream;
      Mlpeg.Success ((Mstream.spos stream), loc)
    end
    else
    begin
      Mstream.pop stream;
      Mlpeg.Fail
    end
    end
  end

and rb stream = 
  begin
    Mstream.push stream;
    let loc = 
    ")"
     in
    begin
    if (Mstream.next stream).r_base = ')' then
    begin
      Mstream.drop stream;
      Mlpeg.Success ((Mstream.spos stream), loc)
    end
    else
    begin
      Mstream.pop stream;
      Mlpeg.Fail
    end
    end
  end

and expr stream = 
  begin
    Mstream.push stream;
    match Mlpeg.apply_rule stream __get_sum __set_sum sum with
      | Mlpeg.Fail -> 
    begin
      Mstream.pop stream;
      Mlpeg.Fail
    end
      | Mlpeg.Success(_, s) -> let loc = s in 
    begin
      Mstream.drop stream;
      Mlpeg.Success ((Mstream.spos stream), loc)
    end
  end

and sum stream = 
  begin
    Mstream.push stream;
    match Mlpeg.apply_rule stream __get_product __set_product product with
      | Mlpeg.Fail -> 
    begin
      Mstream.pop stream;
      Mlpeg.Fail
    end
      | Mlpeg.Success(_, l) -> let loc = l in 
    begin
      Mstream.push stream;
      let res = ref [] in
      let flag = ref true in
      while !flag do
      match Mlpeg.apply_rule stream __get_plus __set_plus plus with
        | Mlpeg.Fail -> 
      begin
        Mstream.pop stream;
        flag := false
      end
        | Mlpeg.Success(_, loc) -> 
      begin
        Mstream.push stream;
        match Mlpeg.apply_rule stream __get_product __set_product product with
          | Mlpeg.Fail -> 
        begin
          Mstream.pop stream;
          match Mlpeg.apply_rule stream __get_minus __set_minus minus with
            | Mlpeg.Fail -> 
          begin
            Mstream.pop stream;
            flag := false
          end
            | Mlpeg.Success(_, loc) -> 
          match Mlpeg.apply_rule stream __get_product __set_product product with
            | Mlpeg.Fail -> 
          begin
            Mstream.pop stream;
            flag := false
          end
            | Mlpeg.Success(_, r) -> let loc = r in 
          let loc = 
           fun x -> x - r 
           in
          begin
          begin
            Mstream.drop stream;
            Mstream.push stream;
            res:=!res @ [loc]
          end
          end
        end
          | Mlpeg.Success(_, r) -> let loc = r in 
        let loc = 
         fun x -> x + r 
         in
        begin
        begin
          Mstream.drop stream;
          Mstream.push stream;
          res:=!res @ [loc]
        end
        end
      end
      done;
      let fncs = !res in
      let loc = 
       List.fold_left (fun acc f -> f acc) l fncs 
       in
      begin
      begin
        Mstream.drop stream;
        Mlpeg.Success ((Mstream.spos stream), loc)
      end
      end
    end
  end

and product stream = 
  begin
    Mstream.push stream;
    match Mlpeg.apply_rule stream __get_value __set_value value with
      | Mlpeg.Fail -> 
    begin
      Mstream.pop stream;
      Mlpeg.Fail
    end
      | Mlpeg.Success(_, l) -> let loc = l in 
    begin
      Mstream.push stream;
      let res = ref [] in
      let flag = ref true in
      while !flag do
      begin
        Mstream.push stream;
        match Mlpeg.apply_rule stream __get_times __set_times times with
          | Mlpeg.Fail -> 
        begin
          Mstream.pop stream;
          match Mlpeg.apply_rule stream __get_divide __set_divide divide with
            | Mlpeg.Fail -> 
          begin
            Mstream.pop stream;
            flag := false
          end
            | Mlpeg.Success(_, loc) -> 
          match Mlpeg.apply_rule stream __get_value __set_value value with
            | Mlpeg.Fail -> 
          begin
            Mstream.pop stream;
            flag := false
          end
            | Mlpeg.Success(_, r) -> let loc = r in 
          let loc = 
           fun x -> x/r
           in
          begin
          begin
            Mstream.drop stream;
            Mstream.push stream;
            res:=!res @ [loc]
          end
          end
        end
          | Mlpeg.Success(_, loc) -> 
        match Mlpeg.apply_rule stream __get_value __set_value value with
          | Mlpeg.Fail -> 
        begin
          Mstream.pop stream;
          match Mlpeg.apply_rule stream __get_divide __set_divide divide with
            | Mlpeg.Fail -> 
          begin
            Mstream.pop stream;
            flag := false
          end
            | Mlpeg.Success(_, loc) -> 
          match Mlpeg.apply_rule stream __get_value __set_value value with
            | Mlpeg.Fail -> 
          begin
            Mstream.pop stream;
            flag := false
          end
            | Mlpeg.Success(_, r) -> let loc = r in 
          let loc = 
           fun x -> x/r
           in
          begin
          begin
            Mstream.drop stream;
            Mstream.push stream;
            res:=!res @ [loc]
          end
          end
        end
          | Mlpeg.Success(_, r) -> let loc = r in 
        let loc = 
         fun x -> x * r
         in
        begin
        begin
          Mstream.drop stream;
          Mstream.push stream;
          res:=!res @ [loc]
        end
        end
      end
      done;
      let fncs = !res in
      let loc = 
       List.fold_left (fun acc f -> f acc) l fncs  
       in
      begin
      begin
        Mstream.drop stream;
        Mlpeg.Success ((Mstream.spos stream), loc)
      end
      end
    end
  end

and value stream = 
  begin
    Mstream.push stream;
    begin
      Mstream.push stream;
      match Mlpeg.apply_rule stream __get_number __set_number number with
        | Mlpeg.Fail -> 
      begin
        Mstream.pop stream;
        match Mlpeg.apply_rule stream __get_lb __set_lb lb with
          | Mlpeg.Fail -> 
        begin
          Mstream.pop stream;
          Mlpeg.Fail
        end
          | Mlpeg.Success(_, loc) -> 
        match Mlpeg.apply_rule stream __get_expr __set_expr expr with
          | Mlpeg.Fail -> 
        begin
          Mstream.pop stream;
          Mlpeg.Fail
        end
          | Mlpeg.Success(_, e) -> let loc = e in 
        match Mlpeg.apply_rule stream __get_rb __set_rb rb with
          | Mlpeg.Fail -> 
        begin
          Mstream.pop stream;
          Mlpeg.Fail
        end
          | Mlpeg.Success(_, loc) -> 
        let loc = 
         e 
         in
        begin
        begin
          Mstream.drop stream;
          Mlpeg.Success ((Mstream.spos stream), loc)
        end
        end
      end
        | Mlpeg.Success(_, i) -> let loc = i in 
      begin
        Mstream.drop stream;
        Mlpeg.Success ((Mstream.spos stream), loc)
      end
    end
  end

and a_main stream = 
  begin
    Mstream.push stream;
    match Mlpeg.apply_rule stream __get_expr __set_expr expr with
      | Mlpeg.Fail -> 
    begin
      Mstream.pop stream;
      Mlpeg.Fail
    end
      | Mlpeg.Success(_, i) -> let loc = i in 
    let loc = 
     string_of_int i 
     in
    begin
    begin
      Mstream.drop stream;
      Mlpeg.Success ((Mstream.spos stream), loc)
    end
    end
  end

let parse str = (Mlpeg.parse memo_table_init str a_main)