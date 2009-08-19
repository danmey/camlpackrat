

type memo = {r_base:char;
  mutable a_main :string Mlpeg.result option;
}

let __set_a_main memo v = memo.a_main <- v 
let __get_a_main memo = memo.a_main

let rec memo_table_init ch = {
  r_base=ch;
  a_main= None;
}

and a_main stream = 
  begin
    Mstream.push stream;
    begin
      Mstream.push stream;
      match Mlpeg.apply_rule stream __get_a_main __set_a_main a_main with
        | Mlpeg.Fail -> 
      begin
        Mstream.pop stream;
        let loc = 
        "1"
         in
        begin
        if (Mstream.next stream).r_base = '1' then
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
        | Mlpeg.Success(_, loc) -> 
      let loc = 
      "+"
       in
      begin
      if (Mstream.next stream).r_base = '+' then
      let loc = 
      "1"
       in
      begin
      if (Mstream.next stream).r_base = '1' then
      begin
        Mstream.drop stream;
        Mlpeg.Success ((Mstream.spos stream), loc)
      end
      else
      begin
        Mstream.pop stream;
        let loc = 
        "1"
         in
        begin
        if (Mstream.next stream).r_base = '1' then
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
      end
      else
      begin
        Mstream.pop stream;
        let loc = 
        "1"
         in
        begin
        if (Mstream.next stream).r_base = '1' then
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
      end
    end
  end

let parse str = (Mlpeg.parse memo_table_init str a_main)